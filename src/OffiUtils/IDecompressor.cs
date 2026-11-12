#if NET11_0_OR_GREATER
using System.Buffers;
using System.Buffers.Binary;
#endif
using System.Collections;
using System.Collections.Frozen;
using System.Diagnostics.CodeAnalysis;
using System.IO.Compression;
using System.Runtime.InteropServices;

namespace OffiUtils;

public interface IDecompressor : IReadOnlyDictionary<CompressionAlgorithm, DecompressDelegate>
{
    /// <summary>
    /// Attempts to decompress the source buffer into the destination buffer.
    /// </summary>
    /// <param name="algorithm">The algorithm to use.</param>
    /// <param name="source">The compressed data.</param>
    /// <param name="destination">The buffer to write decompressed data to.</param>
    /// <param name="bytesWritten">The number of bytes successfully written to the destination, or 0 if decompression failed.</param>
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
public delegate bool DecompressDelegate(
    ReadOnlySpan<byte> source,
    Span<byte> destination,
    out int bytesWritten
);

/// <summary>
/// Represents a stateful decompression method (avoids closures).
/// </summary>
public delegate bool DecompressDelegate<in TState>(
    TState state,
    ReadOnlySpan<byte> source,
    Span<byte> destination,
    out int bytesWritten
);

public sealed class DecompressorBuilder
{
    private readonly Dictionary<CompressionAlgorithm, DecompressDelegate> _handlers = new();

    /// <summary>
    /// Creates a builder with the built-in algorithms registered. On every target framework, their decoders succeed
    /// only if the compressed data is complete and its entire output fits into the destination.
    /// Bytes after the end of the compressed data are ignored, except that further gzip members
    /// and Zstandard frames are decompressed as well.
    /// </summary>
    public static DecompressorBuilder Default => new DecompressorBuilder()
        .Add(CompressionAlgorithm.Deflate, TryDecompressDeflate)
        .Add(CompressionAlgorithm.Gzip, TryDecompressGzip)
        .Add(CompressionAlgorithm.Zlib, TryDecompressZlib)
#if NET11_0_OR_GREATER
        .Add(CompressionAlgorithm.Zstd, TryDecompressZstd)
#endif
        .Add(CompressionAlgorithm.Brotli, BrotliDecoder.TryDecompress);

    public DecompressorBuilder AddRange(IEnumerable<KeyValuePair<CompressionAlgorithm, DecompressDelegate>> values, bool replace = false)
    {
        ArgumentNullException.ThrowIfNull(values);
        foreach ((CompressionAlgorithm algorithm, DecompressDelegate implementation) in values)
        {
            Add(algorithm, implementation, replace);
        }
        return this;
    }

    public DecompressorBuilder Add(CompressionAlgorithm algorithm, DecompressDelegate implementation, bool replace = false)
    {
        ArgumentNullException.ThrowIfNull(implementation);
        if (replace)
        {
            _handlers[algorithm] = implementation;
        }
        else
        {
            _handlers.TryAdd(algorithm, implementation);
        }
        return this;
    }

    public DecompressorBuilder Add<TState>(CompressionAlgorithm algorithm, TState state, DecompressDelegate<TState> implementation, bool replace = false)
    {
        ArgumentNullException.ThrowIfNull(implementation);
        if (replace)
        {
            var wrapper = new DelegateStateWrapper<TState>(state, implementation);
            _handlers[algorithm] = wrapper.Invoke;
        }
        else
        {
            ref DecompressDelegate? valOrDefault = ref CollectionsMarshal.GetValueRefOrAddDefault(_handlers, algorithm, out bool exists);
            if (!exists)
            {
                var wrapper = new DelegateStateWrapper<TState>(state, implementation);
                valOrDefault = wrapper.Invoke;
            }
        }
        return this;
    }

    public IDecompressor Build()
    {
        return new FastDecompressor(_handlers.ToFrozenDictionary());
    }

#if NET11_0_OR_GREATER

    // The static TryDecompress methods of the span decoders also fail when bytes follow the compressed data.
    // The instance API stops at the end of the data, so these use it to get the same results as the stream fallback.

    private static bool TryDecompressDeflate(
        ReadOnlySpan<byte> source,
        Span<byte> destination,
        out int bytesWritten)
    {
        using DeflateDecoder decoder = new();
        return decoder.Decompress(source, destination, out _, out bytesWritten) == OperationStatus.Done;
    }

    private static bool TryDecompressGzip(
        ReadOnlySpan<byte> source,
        Span<byte> destination,
        out int bytesWritten)
    {
        using GZipDecoder decoder = new();
        Span<byte> scratch = stackalloc byte[1];
        bytesWritten = 0;

        while (true)
        {
            // The decoders report DestinationTooSmall for an empty buffer even if nothing is left to write,
            // so once the destination is full they get a one-byte buffer that has to stay empty.
            bool full = bytesWritten == destination.Length;
            Span<byte> output = full ? scratch : destination[bytesWritten..];
            if (decoder.Decompress(source, output, out int bytesConsumed, out int written) != OperationStatus.Done || (full && written != 0))
                return false;

            bytesWritten += written;
            source = source[bytesConsumed..];

            // Like GZipStream, decompress the next member if the remaining input starts like one (RFC 1952, section 2.2).
            if (source.IsEmpty || source[0] != 0x1F || (source.Length > 1 && source[1] != 0x8B))
                return true;

            decoder.Reset();
        }
    }

    private static bool TryDecompressZlib(
        ReadOnlySpan<byte> source,
        Span<byte> destination,
        out int bytesWritten)
    {
        using ZLibDecoder decoder = new();
        return decoder.Decompress(source, destination, out _, out bytesWritten) == OperationStatus.Done;
    }

    private static bool TryDecompressZstd(
        ReadOnlySpan<byte> source,
        Span<byte> destination,
        out int bytesWritten)
    {
        const uint FrameMagic = 0xFD2FB528;
        const uint SkippableFrameMagic = 0x184D2A50; // the lowest 4 bits can have any value

        using ZstandardDecoder decoder = new();
        Span<byte> scratch = stackalloc byte[1];
        bytesWritten = 0;

        while (true)
        {
            // Like in TryDecompressGzip, a full destination gets a one-byte buffer that has to stay empty.
            bool full = bytesWritten == destination.Length;
            Span<byte> output = full ? scratch : destination[bytesWritten..];
            if (decoder.Decompress(source, output, out int bytesConsumed, out int written) != OperationStatus.Done || (full && written != 0))
                return false;

            bytesWritten += written;
            source = source[bytesConsumed..];

            // Like ZstandardStream, decompress the next frame if the remaining input starts with a frame magic number
            // (RFC 8878, sections 3.1.1 and 3.1.2).
            if (source.Length < sizeof(uint))
                return true;

            uint magic = BinaryPrimitives.ReadUInt32LittleEndian(source);
            if (magic != FrameMagic && (magic & ~0xFu) != SkippableFrameMagic)
                return true;

            decoder.Reset();
        }
    }

#else

    private static bool TryDecompressDeflate(
        ReadOnlySpan<byte> source,
        Span<byte> destination,
        out int bytesWritten)
    {
        return TryDecompressStream(source, destination, out bytesWritten, appendSentinel: false,
            static s => new DeflateStream(s, CompressionMode.Decompress, leaveOpen: true));
    }

    private static bool TryDecompressGzip(
        ReadOnlySpan<byte> source,
        Span<byte> destination,
        out int bytesWritten)
    {
        return TryDecompressStream(source, destination, out bytesWritten, appendSentinel: true,
            static s => new GZipStream(s, CompressionMode.Decompress, leaveOpen: true));
    }

    private static bool TryDecompressZlib(
        ReadOnlySpan<byte> source,
        Span<byte> destination,
        out int bytesWritten)
    {
        return TryDecompressStream(source, destination, out bytesWritten, appendSentinel: false,
            static s => new ZLibStream(s, CompressionMode.Decompress, leaveOpen: true));
    }

    private static unsafe bool TryDecompressStream(
        ReadOnlySpan<byte> source,
        Span<byte> destination,
        out int bytesWritten,
        bool appendSentinel,
        Func<Stream, Stream> createDecompressionStream)
    {
        bytesWritten = 0;

        if (source.IsEmpty)
            return false;

        try
        {
            fixed (byte* sourcePtr = source)
            {
                using SourceStream sourceStream = new(sourcePtr, source.Length, appendSentinel);
                using Stream decompressionStream = createDecompressionStream(sourceStream);

                int written = 0;
                while (written < destination.Length)
                {
                    int read = decompressionStream.Read(destination[written..]);
                    if (read == 0)
                        break;
                    written += read;
                }

                // A full destination is only enough if no output is left.
                // Reading into an empty buffer doesn't decompress anything, so this reads one byte.
                if (written == destination.Length)
                {
                    Span<byte> probe = stackalloc byte[1];
                    if (decompressionStream.Read(probe) != 0)
                        return false;
                }

                if (sourceStream.ReadPastEnd)
                    return false;

                bytesWritten = written;
                return true;
            }
        }
        catch
        {
            return false;
        }
    }

    /// <summary>
    /// The source as a stream that records whether the decompression stream wanted more input than the source holds.
    /// </summary>
    /// <remarks>
    /// DeflateStream only reads while the inflater is unfinished and out of input (DeflateStream.ReadCore in .NET 10),
    /// so with complete data it never reads past the end, and with incomplete data it always does.
    /// GZipStream also reads once more after a member that ends exactly at the end of the source, to look for the next one.
    /// The optional sentinel byte answers that read. 0xFF can't start a member, so GZipStream stops there.
    /// If an incomplete member uses up the sentinel as data instead, GZipStream reads again after it,
    /// which counts as a read past the end like any other.
    /// </remarks>
    private sealed unsafe class SourceStream : UnmanagedMemoryStream
    {
        private bool _sentinelPending;

        public SourceStream(byte* source, int length, bool appendSentinel) : base(source, length)
        {
            _sentinelPending = appendSentinel;
        }

        public bool ReadPastEnd { get; private set; }

        // DeflateStream reads through this overload, and UnmanagedMemoryStream.Read(Span<byte>) forwards to it in derived types.
        public override int Read(byte[] buffer, int offset, int count)
        {
            int read = base.Read(buffer, offset, count);
            if (read != 0 || count == 0)
                return read;

            if (_sentinelPending)
            {
                _sentinelPending = false;
                buffer[offset] = 0xFF;
                return 1;
            }

            ReadPastEnd = true;
            return 0;
        }
    }

#endif

    private sealed class DelegateStateWrapper<TState>
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

    private sealed class FastDecompressor : IDecompressor
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
            if (_decompressors.TryGetValue(algorithm, out DecompressDelegate? handler) &&
                handler(source, destination, out bytesWritten) && bytesWritten != 0)
            {
                return true;
            }

            // A handler may report partial output when it fails.
            bytesWritten = 0;
            return false;
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

        public IEnumerator<KeyValuePair<CompressionAlgorithm, DecompressDelegate>> GetEnumerator() => _decompressors.GetEnumerator();

        IEnumerator IEnumerable.GetEnumerator() => _decompressors.GetEnumerator();

        public int Count => _decompressors.Count;
        public bool ContainsKey(CompressionAlgorithm key) => _decompressors.ContainsKey(key);

        public bool TryGetValue(CompressionAlgorithm key, [MaybeNullWhen(false)] out DecompressDelegate value)
            => _decompressors.TryGetValue(key, out value);

        public DecompressDelegate this[CompressionAlgorithm key] => _decompressors[key];

        public IEnumerable<CompressionAlgorithm> Keys => _decompressors.Keys;
        public IEnumerable<DecompressDelegate> Values => _decompressors.Values;
    }
}
