namespace OffiUtils.Tests;

public class JsonUtilsTests
{
	[Fact]
	public void GetUnescapedString()
	{
		var unescaped =
			"""
			{
			  "name": "Elon Musk",
			  "age": 69,
			  "car": "Tesla"
			}
			""".Replace("\r", "");
		var escaped = "{\\n  \\\"name\\\": \\\"Elon Musk\\\",\\n  \\\"age\\\": 69,\\n  \\\"car\\\": \\\"Tesla\\\"\\n}"u8;
		var result = JsonUtils.GetUnescapedString(escaped);
		Assert.Equal(unescaped, result);
	}

	[Fact]
	public void TryUnescapeBytes()
	{
		var unescaped = JsonUtils.Encoding.GetBytes(
			"""
			{
			  "name": "Elon Musk",
			  "age": 69,
			  "car": "Tesla"
			}
			""".Replace("\r", "")).AsSpan();
		var escaped = "{\\n  \\\"name\\\": \\\"Elon Musk\\\",\\n  \\\"age\\\": 69,\\n  \\\"car\\\": \\\"Tesla\\\"\\n}"u8;
		var unescapedBytes = new byte[escaped.Length].AsSpan();

		var result = JsonUtils.TryUnescape(escaped, unescapedBytes, out var written);
		Assert.True(result);
		Assert.Equal(unescaped.Length, written);
		Assert.Equal(unescaped, unescapedBytes[..written]);
	}

	[Fact]
	public void TryUnescapeChars()
	{
		var unescaped =
			"""
			{
			  "name": "Elon Musk",
			  "age": 69,
			  "car": "Tesla"
			}
			""".Replace("\r", "").AsSpan();
		var escaped = "{\\n  \\\"name\\\": \\\"Elon Musk\\\",\\n  \\\"age\\\": 69,\\n  \\\"car\\\": \\\"Tesla\\\"\\n}"u8;
		var unescapedChars = new char[escaped.Length].AsSpan();

		var result = JsonUtils.TryUnescape(escaped, unescapedChars, out var written);
		Assert.True(result);
		Assert.Equal(unescaped.Length, written);
		Assert.Equal(unescaped, unescapedChars[..written]);
	}
}
