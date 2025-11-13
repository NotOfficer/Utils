namespace OffiUtils.Tests;

public class StringUtilsTests
{
    [Fact]
    public void FastAllocate()
    {
        const int length = 32;
        string result = StringUtils.FastAllocate(length);
        Assert.NotNull(result);
        Assert.Equal(length, result.Length);
    }

    [Fact]
    public void RealClone()
    {
        string str = new string('1', 69);
        string clone = StringUtils.RealClone(str);
        Assert.Equal(str, clone, StringComparer.Ordinal);
        Assert.Equal(str, str.Clone(), ReferenceEquals);
        Assert.Equal(clone, clone.Clone(), ReferenceEquals);
        Assert.NotEqual(str, clone, ReferenceEquals);
    }

    [Fact]
    public void BytesToHexLower()
    {
        byte[] bytes = Enumerable.Range(0, 0xFF + 1).Select(x => (byte)x).ToArray();
        string hexString = StringUtils.BytesToHexLower(bytes);
        Assert.Equal(
            BitConverter.ToString(bytes).Replace("-", "").ToLowerInvariant(),
            hexString);
    }

    [Fact]
    public void BytesToHexUpper()
    {
        byte[] bytes = Enumerable.Range(0, 0xFF + 1).Select(x => (byte)x).ToArray();
        string hexString = StringUtils.BytesToHexUpper(bytes);
        Assert.Equal(
            BitConverter.ToString(bytes).Replace("-", ""),
            hexString);
    }

    [Fact]
    public void Random()
    {
        const int length = 32;
        string result = StringUtils.Random(length);
        Assert.NotNull(result);
        Assert.Equal(length, result.Length);
        Assert.All(result, c => Assert.NotEqual('\0', c));
    }

    [Fact]
    public void ToLower()
    {
        string str = new string('A', 3);
        StringUtils.ToLowerAsciiInvariant(str);
        Assert.Equal("aaa", str);
    }

    [Fact]
    public void ToUpper()
    {
        string str = new string('a', 3);
        StringUtils.ToUpperAsciiInvariant(str);
        Assert.Equal("AAA", str);
    }
}
