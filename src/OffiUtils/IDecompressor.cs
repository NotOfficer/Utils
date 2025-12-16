using System.Collections.Frozen;
using System.Diagnostics.CodeAnalysis;
using System.IO.Compression;

namespace OffiUtils;

[Experimental(DiagnosticIds.ExperimentalIDecompressor)]
public interface IDecompressor
{
    /// <summary>
    /// Attempts to decompress the source buffer into the destination buffer.
    /// </summary>
    /// <param name="algorithm">The algorithm to use.</param>
    /// <param name="source">The compressed data.</param>
    /// <param name="destination">The buffer to write decompressed data to.</param>
    /// <param name="bytesWritten">The number of bytes successfully written to the destination.</param>
    /// <returns>True if decompression succeeded; otherwise, false.</returns>
    bool TryDecompress(
        CompressionAlgorithm algorithm,
        ReadOnlySpan<byte> source,
        Span<byte> destination,
        out int bytesWritten
    );

    /// <summary>
    /// Decompress the source buffer into the destination buffer.
    /// </summary>
    /// <param name="algorithm">The algorithm to use.</param>
    /// <param name="source">The compressed data.</param>
    /// <param name="destination">The buffer to write decompressed data to.</param>
    /// <returns>The number of bytes successfully written to the destination.</returns>
    /// <exception cref="NotSupportedException">Thrown if the algorithm is not registered.</exception>
    /// <exception cref="InvalidDataException">Thrown if decompression fails.</exception>
    int Decompress(
        CompressionAlgorithm algorithm,
        ReadOnlySpan<byte> source,
        Span<byte> destination
    );
}

[Experimental(DiagnosticIds.ExperimentalIDecompressor)]
[SuppressMessage("ReSharper", "InconsistentNaming")]
public enum CompressionAlgorithm
{
    Deflate = 1991_01_01,   // PKZIP 2.0 (Phil Katz)
    Gzip    = 1992_10_31,   // 0.1 release (Jean-loup Gailly)
    Zlib    = 1995_05_01,   // 0.9 release (Gailly & Adler)
    LZO     = 1996_01_01,   // Markus Oberhumer
    Bzip2   = 1996_07_18,   // 0.1 release (Julian Seward)
    LZMA    = 1999_01_02,   // 7-Zip initial beta

    SMAZ    = 2009_03_30,   // Antirez release
    LZMA2   = 2009_08_27,   // XZ Utils 4.999.9beta

    Snappy  = 2011_03_23,   // Google open source release
    LZ4     = 2011_04_24,   // r1 release by Yann Collet
    Oodle   = 2012_01_01,   // Estimated early Oodle SDK versions
    Brotli  = 2015_09_22,   // Google "Brotli" initial open source (Generic)
    LZHAM   = 2015_01_24,   // v1.0 release
    LZFSE   = 2015_06_08,   // Apple WWDC 2015 announcement
    Zstd    = 2016_08_31,   // v1.0.0 release by Facebook

    SMAZ2   = 2024_01_30,   // (Estimate for "recent" update in this timeline)
    OpenZL  = 2025_10_06    // Meta announcement date
}

/// <summary>
/// Represents a stateless decompression method.
/// </summary>
[Experimental(DiagnosticIds.ExperimentalIDecompressor)]
public delegate bool DecompressDelegate(
    ReadOnlySpan<byte> source,
    Span<byte> destination,
    out int bytesWritten
);

/// <summary>
/// Represents a stateful decompression method (avoids closures).
/// </summary>
[Experimental(DiagnosticIds.ExperimentalIDecompressor)]
public delegate bool DecompressDelegate<in TState>(
    TState state,
    ReadOnlySpan<byte> source,
    Span<byte> destination,
    out int bytesWritten
);

[Experimental(DiagnosticIds.ExperimentalIDecompressor)]
public sealed class DecompressorBuilder
{
    private readonly Dictionary<CompressionAlgorithm, DecompressDelegate> _handlers = new();

    public static DecompressorBuilder Default => new DecompressorBuilder()
        .Add(CompressionAlgorithm.Deflate, TryDecompressDeflate)
        .Add(CompressionAlgorithm.Gzip, TryDecompressGzip)
        .Add(CompressionAlgorithm.Zlib, TryDecompressZlib)
        .Add(CompressionAlgorithm.Brotli, BrotliDecoder.TryDecompress);

    [Experimental(DiagnosticIds.ExperimentalOodlePort)]
    public static DecompressorBuilder DefaultWithOodlePort => Default
        .Add(CompressionAlgorithm.Oodle, OodleDecompressor.TryDecompress);

    public DecompressorBuilder Add(CompressionAlgorithm algorithm, DecompressDelegate implementation)
    {
        ArgumentNullException.ThrowIfNull(implementation);
        _handlers[algorithm] = implementation;
        return this;
    }

    public DecompressorBuilder Add<TState>(CompressionAlgorithm algorithm, TState state, DecompressDelegate<TState> implementation)
    {
        ArgumentNullException.ThrowIfNull(implementation);
        var wrapper = new DelegateStateWrapper<TState>(state, implementation);
        _handlers[algorithm] = wrapper.Invoke;
        return this;
    }

    public IDecompressor Build()
    {
        return new FastDecompressor(_handlers.ToFrozenDictionary());
    }

    internal static bool TryDecompressDeflate(
        ReadOnlySpan<byte> source,
        Span<byte> destination,
        out int bytesWritten)
    {
        return TryDecompressStream(source, destination, out bytesWritten,
            static s => new DeflateStream(s, CompressionMode.Decompress, leaveOpen: true));
    }

    internal static bool TryDecompressGzip(
        ReadOnlySpan<byte> source,
        Span<byte> destination,
        out int bytesWritten)
    {
        return TryDecompressStream(source, destination, out bytesWritten,
            static s => new GZipStream(s, CompressionMode.Decompress, leaveOpen: true));
    }

    internal static bool TryDecompressZlib(
        ReadOnlySpan<byte> source,
        Span<byte> destination,
        out int bytesWritten)
    {
        return TryDecompressStream(source, destination, out bytesWritten,
            static s => new ZLibStream(s, CompressionMode.Decompress, leaveOpen: true));
    }

    internal static unsafe bool TryDecompressStream(
        ReadOnlySpan<byte> source,
        Span<byte> destination,
        out int bytesWritten,
        Func<UnmanagedMemoryStream, Stream> createDecompressionStream)
    {
        bytesWritten = 0;

        if (source.IsEmpty)
            return true;

        try
        {
            fixed (byte* sourcePtr = source)
            {
                using UnmanagedMemoryStream sourceStream = new(sourcePtr, source.Length);
                using Stream decompressionStream = createDecompressionStream(sourceStream);

                bytesWritten = 0;
                while (bytesWritten < destination.Length)
                {
                    int read = decompressionStream.Read(destination[bytesWritten..]);
                    if (read == 0)
                        break;
                    bytesWritten += read;
                }

                return true;
            }
        }
        catch
        {
            return false;
        }
    }

    internal sealed class DelegateStateWrapper<TState>
    {
        private readonly TState _state;
        private readonly DecompressDelegate<TState> _method;

        public DelegateStateWrapper(TState state, DecompressDelegate<TState> method)
        {
            _state = state;
            _method = method;
        }

        public bool Invoke(ReadOnlySpan<byte> source, Span<byte> destination, out int bytesWritten)
        {
            return _method(_state, source, destination, out bytesWritten);
        }
    }

    internal sealed class FastDecompressor : IDecompressor
    {
        private readonly FrozenDictionary<CompressionAlgorithm, DecompressDelegate> _decompressors;

        public FastDecompressor(FrozenDictionary<CompressionAlgorithm, DecompressDelegate> decompressors)
        {
            _decompressors = decompressors;
        }

        public bool TryDecompress(
            CompressionAlgorithm algorithm,
            ReadOnlySpan<byte> source,
            Span<byte> destination,
            out int bytesWritten)
        {
            bytesWritten = 0;
            return _decompressors.TryGetValue(algorithm, out DecompressDelegate? handler) &&
                   handler(source, destination, out bytesWritten) && bytesWritten != 0;
        }

        public int Decompress(
            CompressionAlgorithm algorithm,
            ReadOnlySpan<byte> source,
            Span<byte> destination)
        {
            if (_decompressors.TryGetValue(algorithm, out DecompressDelegate? handler))
            {
                if (handler(source, destination, out int bytesWritten) && bytesWritten != 0)
                {
                    return bytesWritten;
                }

                throw new InvalidDataException("Decompression failed.");
            }

            throw new NotSupportedException($"Decompression algorithm '{algorithm}' has not been registered with this decompressor.");
        }
    }
}
