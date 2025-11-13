using CommunityToolkit.HighPerformance.Buffers;

namespace OffiUtils.Tests;

public class StringUtilsExtTests
{
    [Fact]
    public void GetRawData()
    {
        string str = new string('1', 3);
        ref char rawData = ref str.GetRawData();
        Assert.Equal('1', rawData);
        rawData = 'a';
        Assert.Equal('a', str[0]);
    }

    [Fact]
    public void GetSpan()
    {
        string str = new string('1', 3);
        Span<char> span = str.GetSpan();
        Assert.Equal(str.AsSpan(), span);
        span.Fill('a');
        Assert.Equal("aaa", str);
        Assert.Equal(str.AsSpan(), span);
    }

    [Fact]
    public void CutAfter_Char()
    {
        const char needle = '_';
        const string value = "123_abc_€$?";
        const string badValue = "123-abc";
        const string result = "abc_€$?";
        const char resultSpanInitChar = '#';

        Span<char> resultSpanInit = stackalloc char[result.Length];
        resultSpanInit.Fill(resultSpanInitChar);

        Span<char> resultSpan = stackalloc char[result.Length];
        string? tryResult;
        ReadOnlySpan<char> trySpanResult;
        int tryWritten;

        // string for value
        Assert.Equal(badValue, badValue.CutAfter(needle));
        Assert.Equal(badValue, badValue.CutAfter(needle, StringPool.Shared));

        Assert.Equal(result, value.CutAfter(needle));
        Assert.Equal(result, value.CutAfter(needle, StringPool.Shared));

        Assert.False(badValue.TryCutAfter(needle, out tryResult));
        Assert.Null(tryResult);
        Assert.False(badValue.TryCutAfter(needle, StringPool.Shared, out tryResult));
        Assert.Null(tryResult);

        Assert.True(value.TryCutAfter(needle, out tryResult));
        Assert.Equal(result, tryResult);
        Assert.True(value.TryCutAfter(needle, StringPool.Shared, out tryResult));
        Assert.Equal(result, tryResult);

        resultSpan.Fill(resultSpanInitChar);
        Assert.False(badValue.TryCutAfter(needle, resultSpan, out tryWritten));
        Assert.Equal(resultSpanInit, resultSpan);
        Assert.Equal(0, tryWritten);

        resultSpan.Fill(resultSpanInitChar);
        Assert.True(value.TryCutAfter(needle, resultSpan, out tryWritten));
        Assert.Equal(result, resultSpan);
        Assert.Equal(resultSpan.Length, tryWritten);

        Assert.Equal(badValue, badValue.CutSpanAfter(needle));
        Assert.Equal(result, value.CutSpanAfter(needle));

        Assert.False(badValue.TryCutSpanAfter(needle, out trySpanResult));
        Assert.Equal(ReadOnlySpan<char>.Empty, trySpanResult);

        Assert.True(value.TryCutSpanAfter(needle, out trySpanResult));
        Assert.Equal(result, trySpanResult);

        // span for value
        Assert.Equal(badValue, badValue.AsSpan().CutAfter(needle));
        Assert.Equal(badValue, badValue.AsSpan().CutAfter(needle, StringPool.Shared));

        Assert.Equal(result, value.AsSpan().CutAfter(needle));
        Assert.Equal(result, value.AsSpan().CutAfter(needle, StringPool.Shared));

        Assert.False(badValue.AsSpan().TryCutAfter(needle, out tryResult));
        Assert.Null(tryResult);
        Assert.False(badValue.AsSpan().TryCutAfter(needle, StringPool.Shared, out tryResult));
        Assert.Null(tryResult);

        Assert.True(value.AsSpan().TryCutAfter(needle, out tryResult));
        Assert.Equal(result, tryResult);
        Assert.True(value.AsSpan().TryCutAfter(needle, StringPool.Shared, out tryResult));
        Assert.Equal(result, tryResult);

        resultSpan.Fill(resultSpanInitChar);
        Assert.False(badValue.AsSpan().TryCutAfter(needle, resultSpan, out tryWritten));
        Assert.Equal(resultSpanInit, resultSpan);
        Assert.Equal(0, tryWritten);

        resultSpan.Fill(resultSpanInitChar);
        Assert.True(value.AsSpan().TryCutAfter(needle, resultSpan, out tryWritten));
        Assert.Equal(result, resultSpan);
        Assert.Equal(resultSpan.Length, tryWritten);

        Assert.Equal(badValue, badValue.CutSpanAfter(needle));
        Assert.Equal(result, value.AsSpan().CutSpanAfter(needle));

        Assert.False(badValue.AsSpan().TryCutSpanAfter(needle, out trySpanResult));
        Assert.Equal(ReadOnlySpan<char>.Empty, trySpanResult);

        Assert.True(value.AsSpan().TryCutSpanAfter(needle, out trySpanResult));
        Assert.Equal(result, trySpanResult);
    }

    [Fact]
    public void CutAfterLast_Char()
    {
        const char needle = '_';
        const string value = "123_abc_€$?";
        const string badValue = "123-abc";
        const string result = "€$?";
        const char resultSpanInitChar = '#';

        Span<char> resultSpanInit = stackalloc char[result.Length];
        resultSpanInit.Fill(resultSpanInitChar);

        Span<char> resultSpan = stackalloc char[result.Length];
        string? tryResult;
        ReadOnlySpan<char> trySpanResult;
        int tryWritten;

        // string for value
        Assert.Equal(badValue, badValue.CutAfterLast(needle));
        Assert.Equal(badValue, badValue.CutAfterLast(needle, StringPool.Shared));

        Assert.Equal(result, value.CutAfterLast(needle));
        Assert.Equal(result, value.CutAfterLast(needle, StringPool.Shared));

        Assert.False(badValue.TryCutAfterLast(needle, out tryResult));
        Assert.Null(tryResult);
        Assert.False(badValue.TryCutAfterLast(needle, StringPool.Shared, out tryResult));
        Assert.Null(tryResult);

        Assert.True(value.TryCutAfterLast(needle, out tryResult));
        Assert.Equal(result, tryResult);
        Assert.True(value.TryCutAfterLast(needle, StringPool.Shared, out tryResult));
        Assert.Equal(result, tryResult);

        resultSpan.Fill(resultSpanInitChar);
        Assert.False(badValue.TryCutAfterLast(needle, resultSpan, out tryWritten));
        Assert.Equal(resultSpanInit, resultSpan);
        Assert.Equal(0, tryWritten);

        resultSpan.Fill(resultSpanInitChar);
        Assert.True(value.TryCutAfterLast(needle, resultSpan, out tryWritten));
        Assert.Equal(result, resultSpan);
        Assert.Equal(resultSpan.Length, tryWritten);

        Assert.Equal(badValue, badValue.CutSpanAfterLast(needle));
        Assert.Equal(result, value.CutSpanAfterLast(needle));

        Assert.False(badValue.TryCutSpanAfterLast(needle, out trySpanResult));
        Assert.Equal(ReadOnlySpan<char>.Empty, trySpanResult);

        Assert.True(value.TryCutSpanAfterLast(needle, out trySpanResult));
        Assert.Equal(result, trySpanResult);

        // span for value
        Assert.Equal(badValue, badValue.AsSpan().CutAfterLast(needle));
        Assert.Equal(badValue, badValue.AsSpan().CutAfterLast(needle, StringPool.Shared));

        Assert.Equal(result, value.AsSpan().CutAfterLast(needle));
        Assert.Equal(result, value.AsSpan().CutAfterLast(needle, StringPool.Shared));

        Assert.False(badValue.AsSpan().TryCutAfterLast(needle, out tryResult));
        Assert.Null(tryResult);
        Assert.False(badValue.AsSpan().TryCutAfterLast(needle, StringPool.Shared, out tryResult));
        Assert.Null(tryResult);

        Assert.True(value.AsSpan().TryCutAfterLast(needle, out tryResult));
        Assert.Equal(result, tryResult);
        Assert.True(value.AsSpan().TryCutAfterLast(needle, StringPool.Shared, out tryResult));
        Assert.Equal(result, tryResult);

        resultSpan.Fill(resultSpanInitChar);
        Assert.False(badValue.AsSpan().TryCutAfterLast(needle, resultSpan, out tryWritten));
        Assert.Equal(resultSpanInit, resultSpan);
        Assert.Equal(0, tryWritten);

        resultSpan.Fill(resultSpanInitChar);
        Assert.True(value.AsSpan().TryCutAfterLast(needle, resultSpan, out tryWritten));
        Assert.Equal(result, resultSpan);
        Assert.Equal(resultSpan.Length, tryWritten);

        Assert.Equal(badValue, badValue.CutSpanAfterLast(needle));
        Assert.Equal(result, value.AsSpan().CutSpanAfterLast(needle));

        Assert.False(badValue.AsSpan().TryCutSpanAfterLast(needle, out trySpanResult));
        Assert.Equal(ReadOnlySpan<char>.Empty, trySpanResult);

        Assert.True(value.AsSpan().TryCutSpanAfterLast(needle, out trySpanResult));
        Assert.Equal(result, trySpanResult);
    }

    [Fact]
    public void CutBefore_Char()
    {
        const char needle = '_';
        const string value = "123_abc_€$?";
        const string badValue = "123-abc";
        const string result = "123";
        const char resultSpanInitChar = '#';

        Span<char> resultSpanInit = stackalloc char[result.Length];
        resultSpanInit.Fill(resultSpanInitChar);

        Span<char> resultSpan = stackalloc char[result.Length];
        string? tryResult;
        ReadOnlySpan<char> trySpanResult;
        int tryWritten;

        // string for value
        Assert.Equal(badValue, badValue.CutBefore(needle));
        Assert.Equal(badValue, badValue.CutBefore(needle, StringPool.Shared));

        Assert.Equal(result, value.CutBefore(needle));
        Assert.Equal(result, value.CutBefore(needle, StringPool.Shared));

        Assert.False(badValue.TryCutBefore(needle, out tryResult));
        Assert.Null(tryResult);
        Assert.False(badValue.TryCutBefore(needle, StringPool.Shared, out tryResult));
        Assert.Null(tryResult);

        Assert.True(value.TryCutBefore(needle, out tryResult));
        Assert.Equal(result, tryResult);
        Assert.True(value.TryCutBefore(needle, StringPool.Shared, out tryResult));
        Assert.Equal(result, tryResult);

        resultSpan.Fill(resultSpanInitChar);
        Assert.False(badValue.TryCutBefore(needle, resultSpan, out tryWritten));
        Assert.Equal(resultSpanInit, resultSpan);
        Assert.Equal(0, tryWritten);

        resultSpan.Fill(resultSpanInitChar);
        Assert.True(value.TryCutBefore(needle, resultSpan, out tryWritten));
        Assert.Equal(result, resultSpan);
        Assert.Equal(resultSpan.Length, tryWritten);

        Assert.Equal(badValue, badValue.CutSpanBefore(needle));
        Assert.Equal(result, value.CutSpanBefore(needle));

        Assert.False(badValue.TryCutSpanBefore(needle, out trySpanResult));
        Assert.Equal(ReadOnlySpan<char>.Empty, trySpanResult);

        Assert.True(value.TryCutSpanBefore(needle, out trySpanResult));
        Assert.Equal(result, trySpanResult);

        // span for value
        Assert.Equal(badValue, badValue.AsSpan().CutBefore(needle));
        Assert.Equal(badValue, badValue.AsSpan().CutBefore(needle, StringPool.Shared));

        Assert.Equal(result, value.AsSpan().CutBefore(needle));
        Assert.Equal(result, value.AsSpan().CutBefore(needle, StringPool.Shared));

        Assert.False(badValue.AsSpan().TryCutBefore(needle, out tryResult));
        Assert.Null(tryResult);
        Assert.False(badValue.AsSpan().TryCutBefore(needle, StringPool.Shared, out tryResult));
        Assert.Null(tryResult);

        Assert.True(value.AsSpan().TryCutBefore(needle, out tryResult));
        Assert.Equal(result, tryResult);
        Assert.True(value.AsSpan().TryCutBefore(needle, StringPool.Shared, out tryResult));
        Assert.Equal(result, tryResult);

        resultSpan.Fill(resultSpanInitChar);
        Assert.False(badValue.AsSpan().TryCutBefore(needle, resultSpan, out tryWritten));
        Assert.Equal(resultSpanInit, resultSpan);
        Assert.Equal(0, tryWritten);

        resultSpan.Fill(resultSpanInitChar);
        Assert.True(value.AsSpan().TryCutBefore(needle, resultSpan, out tryWritten));
        Assert.Equal(result, resultSpan);
        Assert.Equal(resultSpan.Length, tryWritten);

        Assert.Equal(badValue, badValue.CutSpanBefore(needle));
        Assert.Equal(result, value.AsSpan().CutSpanBefore(needle));

        Assert.False(badValue.AsSpan().TryCutSpanBefore(needle, out trySpanResult));
        Assert.Equal(ReadOnlySpan<char>.Empty, trySpanResult);

        Assert.True(value.AsSpan().TryCutSpanBefore(needle, out trySpanResult));
        Assert.Equal(result, trySpanResult);
    }

    [Fact]
    public void CutBeforeLast_Char()
    {
        const char needle = '_';
        const string value = "123_abc_€$?";
        const string badValue = "123-abc";
        const string result = "123_abc";
        const char resultSpanInitChar = '#';

        Span<char> resultSpanInit = stackalloc char[result.Length];
        resultSpanInit.Fill(resultSpanInitChar);

        Span<char> resultSpan = stackalloc char[result.Length];
        string? tryResult;
        ReadOnlySpan<char> trySpanResult;
        int tryWritten;

        // string for value
        Assert.Equal(badValue, badValue.CutBeforeLast(needle));
        Assert.Equal(badValue, badValue.CutBeforeLast(needle, StringPool.Shared));

        Assert.Equal(result, value.CutBeforeLast(needle));
        Assert.Equal(result, value.CutBeforeLast(needle, StringPool.Shared));

        Assert.False(badValue.TryCutBeforeLast(needle, out tryResult));
        Assert.Null(tryResult);
        Assert.False(badValue.TryCutBeforeLast(needle, StringPool.Shared, out tryResult));
        Assert.Null(tryResult);

        Assert.True(value.TryCutBeforeLast(needle, out tryResult));
        Assert.Equal(result, tryResult);
        Assert.True(value.TryCutBeforeLast(needle, StringPool.Shared, out tryResult));
        Assert.Equal(result, tryResult);

        resultSpan.Fill(resultSpanInitChar);
        Assert.False(badValue.TryCutBeforeLast(needle, resultSpan, out tryWritten));
        Assert.Equal(resultSpanInit, resultSpan);
        Assert.Equal(0, tryWritten);

        resultSpan.Fill(resultSpanInitChar);
        Assert.True(value.TryCutBeforeLast(needle, resultSpan, out tryWritten));
        Assert.Equal(result, resultSpan);
        Assert.Equal(resultSpan.Length, tryWritten);

        Assert.Equal(badValue, badValue.CutSpanBeforeLast(needle));
        Assert.Equal(result, value.CutSpanBeforeLast(needle));

        Assert.False(badValue.TryCutSpanBeforeLast(needle, out trySpanResult));
        Assert.Equal(ReadOnlySpan<char>.Empty, trySpanResult);

        Assert.True(value.TryCutSpanBeforeLast(needle, out trySpanResult));
        Assert.Equal(result, trySpanResult);

        // span for value
        Assert.Equal(badValue, badValue.AsSpan().CutBeforeLast(needle));
        Assert.Equal(badValue, badValue.AsSpan().CutBeforeLast(needle, StringPool.Shared));

        Assert.Equal(result, value.AsSpan().CutBeforeLast(needle));
        Assert.Equal(result, value.AsSpan().CutBeforeLast(needle, StringPool.Shared));

        Assert.False(badValue.AsSpan().TryCutBeforeLast(needle, out tryResult));
        Assert.Null(tryResult);
        Assert.False(badValue.AsSpan().TryCutBeforeLast(needle, StringPool.Shared, out tryResult));
        Assert.Null(tryResult);

        Assert.True(value.AsSpan().TryCutBeforeLast(needle, out tryResult));
        Assert.Equal(result, tryResult);
        Assert.True(value.AsSpan().TryCutBeforeLast(needle, StringPool.Shared, out tryResult));
        Assert.Equal(result, tryResult);

        resultSpan.Fill(resultSpanInitChar);
        Assert.False(badValue.AsSpan().TryCutBeforeLast(needle, resultSpan, out tryWritten));
        Assert.Equal(resultSpanInit, resultSpan);
        Assert.Equal(0, tryWritten);

        resultSpan.Fill(resultSpanInitChar);
        Assert.True(value.AsSpan().TryCutBeforeLast(needle, resultSpan, out tryWritten));
        Assert.Equal(result, resultSpan);
        Assert.Equal(resultSpan.Length, tryWritten);

        Assert.Equal(badValue, badValue.CutSpanBeforeLast(needle));
        Assert.Equal(result, value.AsSpan().CutSpanBeforeLast(needle));

        Assert.False(badValue.AsSpan().TryCutSpanBeforeLast(needle, out trySpanResult));
        Assert.Equal(ReadOnlySpan<char>.Empty, trySpanResult);

        Assert.True(value.AsSpan().TryCutSpanBeforeLast(needle, out trySpanResult));
        Assert.Equal(result, trySpanResult);
    }

    [Fact]
    public void CutAfter_String()
    {
        const string needle = "__";
        const string value = "123__abc__€$?";
        const string badValue = "123--abc";
        const string result = "abc__€$?";
        const char resultSpanInitChar = '#';

        Span<char> resultSpanInit = stackalloc char[result.Length];
        resultSpanInit.Fill(resultSpanInitChar);

        Span<char> resultSpan = stackalloc char[result.Length];
        string? tryResult;
        ReadOnlySpan<char> trySpanResult;
        int tryWritten;

        // string for value
        Assert.Equal(badValue, badValue.CutAfter(needle));
        Assert.Equal(badValue, badValue.CutAfter(needle, StringPool.Shared));

        Assert.Equal(result, value.CutAfter(needle));
        Assert.Equal(result, value.CutAfter(needle, StringPool.Shared));

        Assert.False(badValue.TryCutAfter(needle, out tryResult));
        Assert.Null(tryResult);
        Assert.False(badValue.TryCutAfter(needle, StringPool.Shared, out tryResult));
        Assert.Null(tryResult);

        Assert.True(value.TryCutAfter(needle, out tryResult));
        Assert.Equal(result, tryResult);
        Assert.True(value.TryCutAfter(needle, StringPool.Shared, out tryResult));
        Assert.Equal(result, tryResult);

        resultSpan.Fill(resultSpanInitChar);
        Assert.False(badValue.TryCutAfter(needle, resultSpan, out tryWritten));
        Assert.Equal(resultSpanInit, resultSpan);
        Assert.Equal(0, tryWritten);

        resultSpan.Fill(resultSpanInitChar);
        Assert.True(value.TryCutAfter(needle, resultSpan, out tryWritten));
        Assert.Equal(result, resultSpan);
        Assert.Equal(resultSpan.Length, tryWritten);

        Assert.Equal(badValue, badValue.CutSpanAfter(needle));
        Assert.Equal(result, value.CutSpanAfter(needle));

        Assert.False(badValue.TryCutSpanAfter(needle, out trySpanResult));
        Assert.Equal(ReadOnlySpan<char>.Empty, trySpanResult);

        Assert.True(value.TryCutSpanAfter(needle, out trySpanResult));
        Assert.Equal(result, trySpanResult);

        // span for value
        Assert.Equal(badValue, badValue.AsSpan().CutAfter(needle));
        Assert.Equal(badValue, badValue.AsSpan().CutAfter(needle, StringPool.Shared));

        Assert.Equal(result, value.AsSpan().CutAfter(needle));
        Assert.Equal(result, value.AsSpan().CutAfter(needle, StringPool.Shared));

        Assert.False(badValue.AsSpan().TryCutAfter(needle, out tryResult));
        Assert.Null(tryResult);
        Assert.False(badValue.AsSpan().TryCutAfter(needle, StringPool.Shared, out tryResult));
        Assert.Null(tryResult);

        Assert.True(value.AsSpan().TryCutAfter(needle, out tryResult));
        Assert.Equal(result, tryResult);
        Assert.True(value.AsSpan().TryCutAfter(needle, StringPool.Shared, out tryResult));
        Assert.Equal(result, tryResult);

        resultSpan.Fill(resultSpanInitChar);
        Assert.False(badValue.AsSpan().TryCutAfter(needle, resultSpan, out tryWritten));
        Assert.Equal(resultSpanInit, resultSpan);
        Assert.Equal(0, tryWritten);

        resultSpan.Fill(resultSpanInitChar);
        Assert.True(value.AsSpan().TryCutAfter(needle, resultSpan, out tryWritten));
        Assert.Equal(result, resultSpan);
        Assert.Equal(resultSpan.Length, tryWritten);

        Assert.Equal(badValue, badValue.CutSpanAfter(needle));
        Assert.Equal(result, value.AsSpan().CutSpanAfter(needle));

        Assert.False(badValue.AsSpan().TryCutSpanAfter(needle, out trySpanResult));
        Assert.Equal(ReadOnlySpan<char>.Empty, trySpanResult);

        Assert.True(value.AsSpan().TryCutSpanAfter(needle, out trySpanResult));
        Assert.Equal(result, trySpanResult);
    }

    [Fact]
    public void CutAfterLast_String()
    {
        const string needle = "__";
        const string value = "123__abc__€$?";
        const string badValue = "123--abc";
        const string result = "€$?";
        const char resultSpanInitChar = '#';

        Span<char> resultSpanInit = stackalloc char[result.Length];
        resultSpanInit.Fill(resultSpanInitChar);

        Span<char> resultSpan = stackalloc char[result.Length];
        string? tryResult;
        ReadOnlySpan<char> trySpanResult;
        int tryWritten;

        // string for value
        Assert.Equal(badValue, badValue.CutAfterLast(needle));
        Assert.Equal(badValue, badValue.CutAfterLast(needle, StringPool.Shared));

        Assert.Equal(result, value.CutAfterLast(needle));
        Assert.Equal(result, value.CutAfterLast(needle, StringPool.Shared));

        Assert.False(badValue.TryCutAfterLast(needle, out tryResult));
        Assert.Null(tryResult);
        Assert.False(badValue.TryCutAfterLast(needle, StringPool.Shared, out tryResult));
        Assert.Null(tryResult);

        Assert.True(value.TryCutAfterLast(needle, out tryResult));
        Assert.Equal(result, tryResult);
        Assert.True(value.TryCutAfterLast(needle, StringPool.Shared, out tryResult));
        Assert.Equal(result, tryResult);

        resultSpan.Fill(resultSpanInitChar);
        Assert.False(badValue.TryCutAfterLast(needle, resultSpan, out tryWritten));
        Assert.Equal(resultSpanInit, resultSpan);
        Assert.Equal(0, tryWritten);

        resultSpan.Fill(resultSpanInitChar);
        Assert.True(value.TryCutAfterLast(needle, resultSpan, out tryWritten));
        Assert.Equal(result, resultSpan);
        Assert.Equal(resultSpan.Length, tryWritten);

        Assert.Equal(badValue, badValue.CutSpanAfterLast(needle));
        Assert.Equal(result, value.CutSpanAfterLast(needle));

        Assert.False(badValue.TryCutSpanAfterLast(needle, out trySpanResult));
        Assert.Equal(ReadOnlySpan<char>.Empty, trySpanResult);

        Assert.True(value.TryCutSpanAfterLast(needle, out trySpanResult));
        Assert.Equal(result, trySpanResult);

        // span for value
        Assert.Equal(badValue, badValue.AsSpan().CutAfterLast(needle));
        Assert.Equal(badValue, badValue.AsSpan().CutAfterLast(needle, StringPool.Shared));

        Assert.Equal(result, value.AsSpan().CutAfterLast(needle));
        Assert.Equal(result, value.AsSpan().CutAfterLast(needle, StringPool.Shared));

        Assert.False(badValue.AsSpan().TryCutAfterLast(needle, out tryResult));
        Assert.Null(tryResult);
        Assert.False(badValue.AsSpan().TryCutAfterLast(needle, StringPool.Shared, out tryResult));
        Assert.Null(tryResult);

        Assert.True(value.AsSpan().TryCutAfterLast(needle, out tryResult));
        Assert.Equal(result, tryResult);
        Assert.True(value.AsSpan().TryCutAfterLast(needle, StringPool.Shared, out tryResult));
        Assert.Equal(result, tryResult);

        resultSpan.Fill(resultSpanInitChar);
        Assert.False(badValue.AsSpan().TryCutAfterLast(needle, resultSpan, out tryWritten));
        Assert.Equal(resultSpanInit, resultSpan);
        Assert.Equal(0, tryWritten);

        resultSpan.Fill(resultSpanInitChar);
        Assert.True(value.AsSpan().TryCutAfterLast(needle, resultSpan, out tryWritten));
        Assert.Equal(result, resultSpan);
        Assert.Equal(resultSpan.Length, tryWritten);

        Assert.Equal(badValue, badValue.CutSpanAfterLast(needle));
        Assert.Equal(result, value.AsSpan().CutSpanAfterLast(needle));

        Assert.False(badValue.AsSpan().TryCutSpanAfterLast(needle, out trySpanResult));
        Assert.Equal(ReadOnlySpan<char>.Empty, trySpanResult);

        Assert.True(value.AsSpan().TryCutSpanAfterLast(needle, out trySpanResult));
        Assert.Equal(result, trySpanResult);
    }

    [Fact]
    public void CutBefore_String()
    {
        const string needle = "__";
        const string value = "123__abc__€$?";
        const string badValue = "123--abc";
        const string result = "123";
        const char resultSpanInitChar = '#';

        Span<char> resultSpanInit = stackalloc char[result.Length];
        resultSpanInit.Fill(resultSpanInitChar);

        Span<char> resultSpan = stackalloc char[result.Length];
        string? tryResult;
        ReadOnlySpan<char> trySpanResult;
        int tryWritten;

        // string for value
        Assert.Equal(badValue, badValue.CutBefore(needle));
        Assert.Equal(badValue, badValue.CutBefore(needle, StringPool.Shared));

        Assert.Equal(result, value.CutBefore(needle));
        Assert.Equal(result, value.CutBefore(needle, StringPool.Shared));

        Assert.False(badValue.TryCutBefore(needle, out tryResult));
        Assert.Null(tryResult);
        Assert.False(badValue.TryCutBefore(needle, StringPool.Shared, out tryResult));
        Assert.Null(tryResult);

        Assert.True(value.TryCutBefore(needle, out tryResult));
        Assert.Equal(result, tryResult);
        Assert.True(value.TryCutBefore(needle, StringPool.Shared, out tryResult));
        Assert.Equal(result, tryResult);

        resultSpan.Fill(resultSpanInitChar);
        Assert.False(badValue.TryCutBefore(needle, resultSpan, out tryWritten));
        Assert.Equal(resultSpanInit, resultSpan);
        Assert.Equal(0, tryWritten);

        resultSpan.Fill(resultSpanInitChar);
        Assert.True(value.TryCutBefore(needle, resultSpan, out tryWritten));
        Assert.Equal(result, resultSpan);
        Assert.Equal(resultSpan.Length, tryWritten);

        Assert.Equal(badValue, badValue.CutSpanBefore(needle));
        Assert.Equal(result, value.CutSpanBefore(needle));

        Assert.False(badValue.TryCutSpanBefore(needle, out trySpanResult));
        Assert.Equal(ReadOnlySpan<char>.Empty, trySpanResult);

        Assert.True(value.TryCutSpanBefore(needle, out trySpanResult));
        Assert.Equal(result, trySpanResult);

        // span for value
        Assert.Equal(badValue, badValue.AsSpan().CutBefore(needle));
        Assert.Equal(badValue, badValue.AsSpan().CutBefore(needle, StringPool.Shared));

        Assert.Equal(result, value.AsSpan().CutBefore(needle));
        Assert.Equal(result, value.AsSpan().CutBefore(needle, StringPool.Shared));

        Assert.False(badValue.AsSpan().TryCutBefore(needle, out tryResult));
        Assert.Null(tryResult);
        Assert.False(badValue.AsSpan().TryCutBefore(needle, StringPool.Shared, out tryResult));
        Assert.Null(tryResult);

        Assert.True(value.AsSpan().TryCutBefore(needle, out tryResult));
        Assert.Equal(result, tryResult);
        Assert.True(value.AsSpan().TryCutBefore(needle, StringPool.Shared, out tryResult));
        Assert.Equal(result, tryResult);

        resultSpan.Fill(resultSpanInitChar);
        Assert.False(badValue.AsSpan().TryCutBefore(needle, resultSpan, out tryWritten));
        Assert.Equal(resultSpanInit, resultSpan);
        Assert.Equal(0, tryWritten);

        resultSpan.Fill(resultSpanInitChar);
        Assert.True(value.AsSpan().TryCutBefore(needle, resultSpan, out tryWritten));
        Assert.Equal(result, resultSpan);
        Assert.Equal(resultSpan.Length, tryWritten);

        Assert.Equal(badValue, badValue.CutSpanBefore(needle));
        Assert.Equal(result, value.AsSpan().CutSpanBefore(needle));

        Assert.False(badValue.AsSpan().TryCutSpanBefore(needle, out trySpanResult));
        Assert.Equal(ReadOnlySpan<char>.Empty, trySpanResult);

        Assert.True(value.AsSpan().TryCutSpanBefore(needle, out trySpanResult));
        Assert.Equal(result, trySpanResult);
    }

    [Fact]
    public void CutBeforeLast_String()
    {
        const string needle = "__";
        const string value = "123__abc__€$?";
        const string badValue = "123--abc";
        const string result = "123__abc";
        const char resultSpanInitChar = '#';

        Span<char> resultSpanInit = stackalloc char[result.Length];
        resultSpanInit.Fill(resultSpanInitChar);

        Span<char> resultSpan = stackalloc char[result.Length];
        string? tryResult;
        ReadOnlySpan<char> trySpanResult;
        int tryWritten;

        // string for value
        Assert.Equal(badValue, badValue.CutBeforeLast(needle));
        Assert.Equal(badValue, badValue.CutBeforeLast(needle, StringPool.Shared));

        Assert.Equal(result, value.CutBeforeLast(needle));
        Assert.Equal(result, value.CutBeforeLast(needle, StringPool.Shared));

        Assert.False(badValue.TryCutBeforeLast(needle, out tryResult));
        Assert.Null(tryResult);
        Assert.False(badValue.TryCutBeforeLast(needle, StringPool.Shared, out tryResult));
        Assert.Null(tryResult);

        Assert.True(value.TryCutBeforeLast(needle, out tryResult));
        Assert.Equal(result, tryResult);
        Assert.True(value.TryCutBeforeLast(needle, StringPool.Shared, out tryResult));
        Assert.Equal(result, tryResult);

        resultSpan.Fill(resultSpanInitChar);
        Assert.False(badValue.TryCutBeforeLast(needle, resultSpan, out tryWritten));
        Assert.Equal(resultSpanInit, resultSpan);
        Assert.Equal(0, tryWritten);

        resultSpan.Fill(resultSpanInitChar);
        Assert.True(value.TryCutBeforeLast(needle, resultSpan, out tryWritten));
        Assert.Equal(result, resultSpan);
        Assert.Equal(resultSpan.Length, tryWritten);

        Assert.Equal(badValue, badValue.CutSpanBeforeLast(needle));
        Assert.Equal(result, value.CutSpanBeforeLast(needle));

        Assert.False(badValue.TryCutSpanBeforeLast(needle, out trySpanResult));
        Assert.Equal(ReadOnlySpan<char>.Empty, trySpanResult);

        Assert.True(value.TryCutSpanBeforeLast(needle, out trySpanResult));
        Assert.Equal(result, trySpanResult);

        // span for value
        Assert.Equal(badValue, badValue.AsSpan().CutBeforeLast(needle));
        Assert.Equal(badValue, badValue.AsSpan().CutBeforeLast(needle, StringPool.Shared));

        Assert.Equal(result, value.AsSpan().CutBeforeLast(needle));
        Assert.Equal(result, value.AsSpan().CutBeforeLast(needle, StringPool.Shared));

        Assert.False(badValue.AsSpan().TryCutBeforeLast(needle, out tryResult));
        Assert.Null(tryResult);
        Assert.False(badValue.AsSpan().TryCutBeforeLast(needle, StringPool.Shared, out tryResult));
        Assert.Null(tryResult);

        Assert.True(value.AsSpan().TryCutBeforeLast(needle, out tryResult));
        Assert.Equal(result, tryResult);
        Assert.True(value.AsSpan().TryCutBeforeLast(needle, StringPool.Shared, out tryResult));
        Assert.Equal(result, tryResult);

        resultSpan.Fill(resultSpanInitChar);
        Assert.False(badValue.AsSpan().TryCutBeforeLast(needle, resultSpan, out tryWritten));
        Assert.Equal(resultSpanInit, resultSpan);
        Assert.Equal(0, tryWritten);

        resultSpan.Fill(resultSpanInitChar);
        Assert.True(value.AsSpan().TryCutBeforeLast(needle, resultSpan, out tryWritten));
        Assert.Equal(result, resultSpan);
        Assert.Equal(resultSpan.Length, tryWritten);

        Assert.Equal(badValue, badValue.CutSpanBeforeLast(needle));
        Assert.Equal(result, value.AsSpan().CutSpanBeforeLast(needle));

        Assert.False(badValue.AsSpan().TryCutSpanBeforeLast(needle, out trySpanResult));
        Assert.Equal(ReadOnlySpan<char>.Empty, trySpanResult);

        Assert.True(value.AsSpan().TryCutSpanBeforeLast(needle, out trySpanResult));
        Assert.Equal(result, trySpanResult);
    }
}
