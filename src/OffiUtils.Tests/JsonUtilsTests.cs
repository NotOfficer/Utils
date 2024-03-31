namespace OffiUtils.Tests;

public class JsonUtilsTests
{
	[Fact]
	public void TryUnescape()
	{
		const string unescaped =
			"""
			{
			  "name": "Elon Musk",
			  "age": 69,
			  "car": "Tesla"
			}
			""";
		const string escaped = "{\\n  \\\"name\\\": \\\"Elon Musk\\\",\\n  \\\"age\\\": 69,\\n  \\\"car\\\": \\\"Tesla\\\"\\n}";
		var escapedBytes = JsonUtils.Encoding.GetBytes(escaped);
		var result = JsonUtils.GetUnescapedString(escapedBytes);
		Assert.Equal(unescaped.Replace("\r", ""), result);
	}
}
