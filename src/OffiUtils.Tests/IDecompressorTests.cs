using System.Buffers;
using System.Diagnostics.CodeAnalysis;
using System.IO.Compression;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace OffiUtils.Tests;

public class DecompressorTests
{
    private static ReadOnlySpan<byte> RawData
        => "0000000000000000______6666666666666666______9999999999999999"u8;

    private static IDecompressor DefaultDecompressor => DecompressorBuilder.Default.Build();

    public static TheoryData<CompressionAlgorithm> DefaultAlgorithms =>
    [
        CompressionAlgorithm.Deflate,
        CompressionAlgorithm.Gzip,
        CompressionAlgorithm.Zlib,
        CompressionAlgorithm.Brotli,
#if NET11_0_OR_GREATER
        CompressionAlgorithm.Zstd,
#endif
    ];

    private static byte[] Compress(CompressionAlgorithm algorithm, ReadOnlySpan<byte> data)
    {
        using MemoryStream output = new();
        using (Stream compressor = algorithm switch
        {
            CompressionAlgorithm.Deflate => new DeflateStream(output, CompressionLevel.Optimal, leaveOpen: true),
            CompressionAlgorithm.Gzip => new GZipStream(output, CompressionLevel.Optimal, leaveOpen: true),
            CompressionAlgorithm.Zlib => new ZLibStream(output, CompressionLevel.Optimal, leaveOpen: true),
            CompressionAlgorithm.Brotli => new BrotliStream(output, CompressionLevel.Optimal, leaveOpen: true),
#if NET11_0_OR_GREATER
            CompressionAlgorithm.Zstd => new ZstandardStream(output, CompressionLevel.Optimal, leaveOpen: true),
#endif
            _ => throw new ArgumentOutOfRangeException(nameof(algorithm), algorithm, null),
        })
        {
            compressor.Write(data);
        }
        return output.ToArray();
    }

    private static void AssertFails(CompressionAlgorithm algorithm, ReadOnlySpan<byte> source, Span<byte> destination)
    {
        Assert.False(DefaultDecompressor.TryDecompress(algorithm, source, destination, out int bytesWritten));
        Assert.Equal(0, bytesWritten);
    }

    [Fact]
    public void NotSupported()
    {
        Assert.Throws<NotSupportedException>(()
            => DefaultDecompressor.Decompress(0, default, default));
    }

    [Fact]
    public void TryNotSupported()
    {
        Assert.False(DefaultDecompressor.TryDecompress(0, default, default, out _));
    }

    private static unsafe void TryStream
        <[DynamicallyAccessedMembers(
            DynamicallyAccessedMemberTypes.PublicConstructors |
            DynamicallyAccessedMemberTypes.NonPublicConstructors)] TStream>
        () where TStream : Stream
    {
        Type tStreamType = typeof(TStream);
        ConstructorInfo? ctorInfo = tStreamType.GetConstructor([typeof(Stream), typeof(CompressionLevel), typeof(bool)]);
        Assert.NotNull(ctorInfo);

        Assert.True(Enum.TryParse(tStreamType.Name.AsSpan()[..^"Stream".Length], true,
            out CompressionAlgorithm algorithm), "Failed to parse CompressionAlgorithm");

        RuntimeHelpers.PrepareMethod(ctorInfo.MethodHandle);
        delegate* managed<TStream, Stream, CompressionLevel, bool, void> ctor =
            (delegate* managed<TStream, Stream, CompressionLevel, bool, void>)ctorInfo.MethodHandle.GetFunctionPointer();

        Span<byte> stackBuffer = stackalloc byte[128];
        UnmanagedMemoryStream compressedStream = MemoryUtils.ToMemoryStream(stackBuffer, FileAccess.ReadWrite);
        TStream tStream = (TStream)RuntimeHelpers.GetUninitializedObject(tStreamType);
        ctor(tStream, compressedStream, CompressionLevel.SmallestSize, true);

        using (tStream)
        {
            tStream.Write(RawData);
        }

        Assert.InRange(compressedStream.Position, 1, 64);
        ReadOnlySpan<byte> compressedData = stackBuffer.Slice(0, (int)compressedStream.Position);

        Assert.True(DefaultDecompressor.TryDecompress(
            algorithm,
            compressedData, stackBuffer.Slice(64),
            out int decompressedDataSize), "TryDecompress failed");

        ReadOnlySpan<byte> decompressedData = stackBuffer.Slice(64, decompressedDataSize);
        Assert.Equal(RawData, decompressedData);
    }

    [Fact]
    public void TryDeflate() => TryStream<DeflateStream>();
    [Fact]
    public void TryGzip() => TryStream<GZipStream>();
    [Fact]
    public void TryZlib() => TryStream<ZLibStream>();

    [Fact]
    public void TryBrotli()
    {
        Span<byte> stackBuffer = stackalloc byte[128];

        Assert.True(BrotliEncoder.TryCompress(
            RawData, stackBuffer.Slice(0, 64),
            out int compressedDataSize, 11, 24));
        ReadOnlySpan<byte> compressedData = stackBuffer.Slice(0, compressedDataSize);

        Assert.True(DefaultDecompressor.TryDecompress(
            CompressionAlgorithm.Brotli,
            compressedData, stackBuffer.Slice(64),
            out int decompressedDataSize));

        ReadOnlySpan<byte> decompressedData = stackBuffer.Slice(64, decompressedDataSize);
        Assert.Equal(RawData, decompressedData);
    }

    [Fact]
    public void TryBrotliStateful()
    {
        Span<byte> stackBuffer = stackalloc byte[128];

        Assert.True(BrotliEncoder.TryCompress(
            RawData, stackBuffer.Slice(0, 64),
            out int compressedDataSize, 11, 24));
        ReadOnlySpan<byte> compressedData = stackBuffer.Slice(0, compressedDataSize);

        IDecompressor decompressor = new DecompressorBuilder()
            .Add(CompressionAlgorithm.Brotli, new BrotliDecoder(),
                (decoder, src, dst, out bw)
                    => decoder.Decompress(src, dst, out _, out bw) == OperationStatus.Done)
            .Build();

        Assert.True(decompressor.TryDecompress(
            CompressionAlgorithm.Brotli,
            compressedData, stackBuffer.Slice(64),
            out int decompressedDataSize));

        ReadOnlySpan<byte> decompressedData = stackBuffer.Slice(64, decompressedDataSize);
        Assert.Equal(RawData, decompressedData);
    }

    [Theory]
    [MemberData(nameof(DefaultAlgorithms))]
    public void TryLargeData(CompressionAlgorithm algorithm)
    {
        // Big enough that the net10.0 stream fallback reads the source in many chunks. Half random, half zeros.
        byte[] data = new byte[256 * 1024];
        new Random(1337).NextBytes(data.AsSpan(0, data.Length / 2));
        byte[] destination = new byte[data.Length];

        Assert.Equal(data.Length, DefaultDecompressor.Decompress(algorithm, Compress(algorithm, data), destination));
        Assert.Equal(data, destination);
    }

    [Theory]
    [MemberData(nameof(DefaultAlgorithms))]
    public void TryTrailingBytes(CompressionAlgorithm algorithm)
    {
        byte[] source = [.. Compress(algorithm, RawData), .. "bytes after the compressed data are ignored"u8];
        Span<byte> destination = stackalloc byte[128];

        Assert.True(DefaultDecompressor.TryDecompress(algorithm, source, destination, out int bytesWritten));
        Assert.Equal(RawData, destination.Slice(0, bytesWritten));
    }

    [Theory]
    [MemberData(nameof(DefaultAlgorithms))]
    public void TryInvalid(CompressionAlgorithm algorithm)
    {
        byte[] compressed = Compress(algorithm, RawData);
        byte[] destination = new byte[RawData.Length];

        AssertFails(algorithm, "not compressed"u8, destination);
        AssertFails(algorithm, [], destination);
        AssertFails(algorithm, compressed.AsSpan(..^1), destination); // incomplete source
        AssertFails(algorithm, compressed, destination.AsSpan(..^1)); // destination too small

        Assert.Throws<InvalidDataException>(()
            => DefaultDecompressor.Decompress(algorithm, compressed, destination.AsSpan(..^1)));
    }

    [Fact]
    public void TryGzipMembers()
    {
        // Like GZipStream, every gzip member is decompressed (RFC 1952, section 2.2).
        byte[] member = Compress(CompressionAlgorithm.Gzip, RawData);
        // Header, an empty fixed Huffman block, then CRC32 and ISIZE of 0.
        byte[] emptyMember = [0x1F, 0x8B, 0x08, 0, 0, 0, 0, 0, 0, 0xFF, 0x03, 0, 0, 0, 0, 0, 0, 0, 0, 0];
        byte[] expected = [.. RawData, .. RawData];
        Span<byte> destination = stackalloc byte[expected.Length];

        Assert.True(DefaultDecompressor.TryDecompress(CompressionAlgorithm.Gzip, [.. member, .. member], destination, out int bytesWritten));
        Assert.Equal(expected, destination.Slice(0, bytesWritten));

        // An empty member still fits into a full destination.
        Assert.True(DefaultDecompressor.TryDecompress(CompressionAlgorithm.Gzip, [.. member, .. emptyMember], destination.Slice(0, RawData.Length), out bytesWritten));
        Assert.Equal(RawData, destination.Slice(0, bytesWritten));

        // Bytes that start like a member are decompressed as one, so they have to be complete.
        AssertFails(CompressionAlgorithm.Gzip, [.. member, 0x1F, 0x8B], destination);
    }

#if NET11_0_OR_GREATER

    [Fact]
    public void TryZstdFrames()
    {
        // Like ZstandardStream, every frame is decompressed, skippable frames included (RFC 8878, section 3.1).
        byte[] frame = Compress(CompressionAlgorithm.Zstd, RawData);
        byte[] skippableFrame = [0x5E, 0x2A, 0x4D, 0x18, 0x04, 0x00, 0x00, 0x00, .. "meta"u8];
        byte[] expected = [.. RawData, .. RawData];
        Span<byte> destination = stackalloc byte[expected.Length];

        Assert.True(DefaultDecompressor.TryDecompress(CompressionAlgorithm.Zstd,
            [.. skippableFrame, .. frame, .. skippableFrame, .. frame, .. skippableFrame], destination, out int bytesWritten));
        Assert.Equal(expected, destination.Slice(0, bytesWritten));

        AssertFails(CompressionAlgorithm.Zstd, [.. frame, .. frame.AsSpan(..^1)], destination);
    }

    [Theory]
    [InlineData(CompressionAlgorithm.Deflate)]
    [InlineData(CompressionAlgorithm.Gzip)]
    [InlineData(CompressionAlgorithm.Zlib)]
    [InlineData(CompressionAlgorithm.Zstd)]
    public void TryEncoder(CompressionAlgorithm algorithm)
    {
        Span<byte> stackBuffer = stackalloc byte[128];

        int compressedDataSize = 0;
        Assert.True(algorithm switch
        {
            CompressionAlgorithm.Deflate => DeflateEncoder.TryCompress(RawData, stackBuffer.Slice(0, 64), out compressedDataSize),
            CompressionAlgorithm.Gzip => GZipEncoder.TryCompress(RawData, stackBuffer.Slice(0, 64), out compressedDataSize),
            CompressionAlgorithm.Zlib => ZLibEncoder.TryCompress(RawData, stackBuffer.Slice(0, 64), out compressedDataSize),
            CompressionAlgorithm.Zstd => ZstandardEncoder.TryCompress(RawData, stackBuffer.Slice(0, 64), out compressedDataSize),
            _ => false,
        });
        ReadOnlySpan<byte> compressedData = stackBuffer.Slice(0, compressedDataSize);

        Assert.True(DefaultDecompressor.TryDecompress(
            algorithm,
            compressedData, stackBuffer.Slice(64),
            out int decompressedDataSize));

        ReadOnlySpan<byte> decompressedData = stackBuffer.Slice(64, decompressedDataSize);
        Assert.Equal(RawData, decompressedData);
    }

#endif
}
