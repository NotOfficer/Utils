using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Running;

using OffiUtils;

BenchmarkRunner.Run<Benchmarks>();

[MemoryDiagnoser(false)]
public class Benchmarks
{
    private byte[] _bytes = null!;
    private char[] _chars = null!;

    [GlobalSetup]
    public void Setup()
    {
        _bytes = Enumerable.Range(0, 256).Select(x => (byte)x).ToArray();
        _chars = new char[_bytes.Length * 2];
    }

    [Benchmark]
    public bool BytesToHexLower()
    {
        return StringUtils.TryWriteBytesToHexLower(_bytes, _chars, out _);
    }

    [Benchmark]
    public bool BytesToHexUpper()
    {
        return StringUtils.TryWriteBytesToHexUpper(_bytes, _chars, out _);
    }
}
