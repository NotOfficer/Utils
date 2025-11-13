namespace OffiUtils.Tests;

public class MemoryUtilsTests
{
    [Fact]
    public void AsSpan()
    {
        var array = new[] {0x112233, 0x445566, 0x778899};
        var readonlySpan = new ReadOnlySpan<int>(array);
        var span = MemoryUtils.AsSpan(readonlySpan);
        Assert.Equal(array.Length, span.Length);
        span.Fill(0x69_420);
        Assert.All(array, x => Assert.Equal(0x69_420, x));
    }
}
