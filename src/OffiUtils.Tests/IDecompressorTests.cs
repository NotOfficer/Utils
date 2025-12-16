using System.Buffers;
using System.Diagnostics.CodeAnalysis;
using System.IO.Compression;
using System.Reflection;
using System.Runtime.CompilerServices;

#pragma warning disable DECOM001

namespace OffiUtils.Tests;

public class DecompressorTests
{
    private static ReadOnlySpan<byte> RawData
        => "0000000000000000______6666666666666666______9999999999999999"u8;

    private static IDecompressor DefaultDecompressor => DecompressorBuilder.Default.Build();

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
#if NET10_0_OR_GREATER
                (decoder, src, dst, out bw)
#else
                (BrotliDecoder decoder, ReadOnlySpan<byte> src, Span<byte> dst, out int bw)
#endif
                    => decoder.Decompress(src, dst, out _, out bw) == OperationStatus.Done)
            .Build();

        Assert.True(decompressor.TryDecompress(
            CompressionAlgorithm.Brotli,
            compressedData, stackBuffer.Slice(64),
            out int decompressedDataSize));

        ReadOnlySpan<byte> decompressedData = stackBuffer.Slice(64, decompressedDataSize);
        Assert.Equal(RawData, decompressedData);
    }
}
