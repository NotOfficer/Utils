using Xunit.Abstractions;

namespace OffiUtils.Tests;

public class StringUtilsTests
{
	private readonly ITestOutputHelper _outputHelper;

	public StringUtilsTests(ITestOutputHelper outputHelper)
	{
		_outputHelper = outputHelper;
	}

	[Fact]
	public void FastAllocate()
	{
		const int length = 32;
		var result = StringUtils.FastAllocate(length);
		Assert.NotNull(result);
		Assert.Equal(length, result.Length);
	}

	[Fact]
	public void GetRawData()
	{
		var str = StringUtils.RealClone("123");
		ref var rawData = ref StringUtils.GetRawData(str);
		Assert.Equal('1', rawData);
		rawData = 'a';
		Assert.Equal('a', str[0]);
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
		Assert.False(result.All(x => x == '\0'));
	}

	[Fact]
	public void ToLower()
	{
		var str = StringUtils.RealClone("ABC");
		StringUtils.ToLowerAsciiInvariant(str);
		Assert.Equal("abc", str);
	}

	[Fact]
	public void ToUpper()
	{
		var str = StringUtils.RealClone("abc");
		StringUtils.ToUpperAsciiInvariant(str);
		Assert.Equal("ABC", str);
	}
}