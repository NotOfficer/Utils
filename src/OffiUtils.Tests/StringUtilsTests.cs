namespace OffiUtils.Tests;

public class StringUtilsTests
{
	[Fact]
	public void FastAllocate()
	{
		const int length = 32;
		var result = StringUtils.FastAllocate(length);
		Assert.NotNull(result);
		Assert.Equal(length, result.Length);
	}

	[Fact]
	public void RealClone()
	{
		var str = new string('1', 69);
		var clone = StringUtils.RealClone(str);
		Assert.Equal(str, clone, StringComparer.Ordinal);
		Assert.Equal(str, str.Clone(), ReferenceEquals);
		Assert.Equal(clone, clone.Clone(), ReferenceEquals);
		Assert.NotEqual(str, clone, ReferenceEquals);
	}

	[Fact]
	public void BytesToHexLower()
	{
		var bytes = Enumerable.Range(0, 0xFF + 1).Select(x => (byte)x).ToArray();
		var hexString = StringUtils.BytesToHexLower(bytes);
		Assert.Equal(
			BitConverter.ToString(bytes).Replace("-", "").ToLowerInvariant(),
			hexString);
	}

	[Fact]
	public void BytesToHexUpper()
	{
		var bytes = Enumerable.Range(0, 0xFF + 1).Select(x => (byte)x).ToArray();
		var hexString = StringUtils.BytesToHexUpper(bytes);
		Assert.Equal(
			BitConverter.ToString(bytes).Replace("-", ""),
			hexString);
	}

	[Fact]
	public void Random()
	{
		const int length = 32;
		var result = StringUtils.Random(length);
		Assert.NotNull(result);
		Assert.Equal(length, result.Length);
		Assert.All(result, c => Assert.NotEqual('\0', c));
	}

	[Fact]
	public void ToLower()
	{
		var str = new string('A', 3);
		StringUtils.ToLowerAsciiInvariant(str);
		Assert.Equal("aaa", str);
	}

	[Fact]
	public void ToUpper()
	{
		var str = new string('a', 3);
		StringUtils.ToUpperAsciiInvariant(str);
		Assert.Equal("AAA", str);
	}
}
