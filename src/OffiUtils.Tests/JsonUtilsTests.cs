namespace OffiUtils.Tests;

public class JsonUtilsTests
{
    [Fact]
    public void GetUnescapedString()
    {
        string unescaped =
            """
            {
              "name": "Elon Musk",
              "age": 69,
              "car": "Tesla"
            }
            """.Replace("\r", "");
        var escaped = "{\\n  \\\"name\\\": \\\"Elon Musk\\\",\\n  \\\"age\\\": 69,\\n  \\\"car\\\": \\\"Tesla\\\"\\n}"u8;
        string result = JsonUtils.GetUnescapedString(escaped);
        Assert.Equal(unescaped, result);
    }

    [Fact]
    public void TryUnescapeBytes()
    {
        Span<byte> unescaped = JsonUtils.Encoding.GetBytes(
            """
            {
              "name": "Elon Musk",
              "age": 69,
              "car": "Tesla"
            }
            """.Replace("\r", "")).AsSpan();
        var escaped = "{\\n  \\\"name\\\": \\\"Elon Musk\\\",\\n  \\\"age\\\": 69,\\n  \\\"car\\\": \\\"Tesla\\\"\\n}"u8;
        Span<byte> unescapedBytes = new byte[escaped.Length].AsSpan();

        bool result = JsonUtils.TryUnescape(escaped, unescapedBytes, out int written);
        Assert.True(result);
        Assert.Equal(unescaped.Length, written);
        Assert.Equal(unescaped, unescapedBytes[..written]);
    }

    [Fact]
    public void TryUnescapeChars()
    {
        ReadOnlySpan<char> unescaped =
            """
            {
              "name": "Elon Musk",
              "age": 69,
              "car": "Tesla"
            }
            """.Replace("\r", "").AsSpan();
        var escaped = "{\\n  \\\"name\\\": \\\"Elon Musk\\\",\\n  \\\"age\\\": 69,\\n  \\\"car\\\": \\\"Tesla\\\"\\n}"u8;
        Span<char> unescapedChars = new char[escaped.Length].AsSpan();

        bool result = JsonUtils.TryUnescape(escaped, unescapedChars, out int written);
        Assert.True(result);
        Assert.Equal(unescaped.Length, written);
        Assert.Equal(unescaped, unescapedChars[..written]);
    }
}
