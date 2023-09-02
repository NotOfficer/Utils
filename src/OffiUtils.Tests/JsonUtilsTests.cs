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
		const string escaped = "{\\r\\n  \\\"name\\\": \\\"Elon Musk\\\",\\r\\n  \\\"age\\\": 69,\\r\\n  \\\"car\\\": \\\"Tesla\\\"\\r\\n}";
		var escapedBytes = JsonUtils.Encoding.GetBytes(escaped);
		var result = JsonUtils.GetUnescapedString(escapedBytes);
		Assert.Equal(unescaped, result);
	}
}