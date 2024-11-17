namespace OffiUtils.Tests;

public class JsonUtilsTests
{
	[Fact]
	public void TryUnescape()
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
}
