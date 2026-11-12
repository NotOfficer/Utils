using System.Globalization;

using CommunityToolkit.HighPerformance.Buffers;

#pragma warning disable IDE0018

namespace OffiUtils.Tests;

public class StringUtilsExtTests
{
    [Fact]
    public void GetRawData()
    {
        string str = new('1', 3);
        ref char rawData = ref str.GetRawData();
        Assert.Equal('1', rawData);
        rawData = 'a';
        Assert.Equal('a', str[0]);
    }

    [Fact]
    public void GetSpan()
    {
        string str = new('1', 3);
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

        Assert.Equal(badValue, badValue.AsSpan().CutSpanAfter(needle));
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

        Assert.Equal(badValue, badValue.AsSpan().CutSpanAfterLast(needle));
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

        Assert.Equal(badValue, badValue.AsSpan().CutSpanBefore(needle));
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

        Assert.Equal(badValue, badValue.AsSpan().CutSpanBeforeLast(needle));
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

        Assert.Equal(badValue, badValue.AsSpan().CutSpanAfter(needle));
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

        Assert.Equal(badValue, badValue.AsSpan().CutSpanAfterLast(needle));
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

        Assert.Equal(badValue, badValue.AsSpan().CutSpanBefore(needle));
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

        Assert.Equal(badValue, badValue.AsSpan().CutSpanBeforeLast(needle));
        Assert.Equal(result, value.AsSpan().CutSpanBeforeLast(needle));

        Assert.False(badValue.AsSpan().TryCutSpanBeforeLast(needle, out trySpanResult));
        Assert.Equal(ReadOnlySpan<char>.Empty, trySpanResult);

        Assert.True(value.AsSpan().TryCutSpanBeforeLast(needle, out trySpanResult));
        Assert.Equal(result, trySpanResult);
    }

    [Fact]
    public void CutAfter_String_Comparison()
    {
        const string needle = "AB";
        const string value = "123_ab_456_Ab_789";
        const string result = "_456_Ab_789";
        const char resultSpanInitChar = '#';

        Span<char> resultSpanInit = stackalloc char[result.Length];
        resultSpanInit.Fill(resultSpanInitChar);

        Span<char> resultSpan = stackalloc char[result.Length];
        Span<char> smallResultSpan = stackalloc char[result.Length - 1];
        StringPool pool = new();
        string? tryResult;
        ReadOnlySpan<char> trySpanResult;
        int tryWritten;

        // string for value
        // the needle only matches when ignoring case, so overloads without a comparison must not match
        Assert.Equal(value, value.CutAfter(needle));
        Assert.Equal(value, value.CutAfter(needle, pool));
        Assert.False(value.TryCutAfter(needle, out tryResult));
        Assert.False(value.TryCutAfter(needle, pool, out tryResult));
        Assert.False(value.TryCutAfter(needle, resultSpan, out tryWritten));
        Assert.Equal(value, value.CutSpanAfter(needle));
        Assert.False(value.TryCutSpanAfter(needle, out trySpanResult));

        Assert.Equal(value, value.CutAfter(needle, StringComparison.Ordinal));
        Assert.Equal(value, value.CutAfter(needle, StringComparison.Ordinal, pool));

        Assert.Equal(result, value.CutAfter(needle, StringComparison.OrdinalIgnoreCase));
        Assert.Same(pool.GetOrAdd(result), value.CutAfter(needle, StringComparison.OrdinalIgnoreCase, pool));

        Assert.False(value.TryCutAfter(needle, StringComparison.Ordinal, out tryResult));
        Assert.Null(tryResult);
        Assert.False(value.TryCutAfter(needle, StringComparison.Ordinal, pool, out tryResult));
        Assert.Null(tryResult);

        Assert.True(value.TryCutAfter(needle, StringComparison.OrdinalIgnoreCase, out tryResult));
        Assert.Equal(result, tryResult);
        Assert.True(value.TryCutAfter(needle, StringComparison.OrdinalIgnoreCase, pool, out tryResult));
        Assert.Same(pool.GetOrAdd(result), tryResult);

        resultSpan.Fill(resultSpanInitChar);
        Assert.False(value.TryCutAfter(needle, StringComparison.Ordinal, resultSpan, out tryWritten));
        Assert.Equal(resultSpanInit, resultSpan);
        Assert.Equal(0, tryWritten);

        resultSpan.Fill(resultSpanInitChar);
        Assert.True(value.TryCutAfter(needle, StringComparison.OrdinalIgnoreCase, resultSpan, out tryWritten));
        Assert.Equal(result, resultSpan);
        Assert.Equal(resultSpan.Length, tryWritten);

        smallResultSpan.Fill(resultSpanInitChar);
        Assert.False(value.TryCutAfter(needle, StringComparison.OrdinalIgnoreCase, smallResultSpan, out tryWritten));
        Assert.Equal(resultSpanInit[..smallResultSpan.Length], smallResultSpan);
        Assert.Equal(0, tryWritten);

        Assert.Equal(value, value.CutSpanAfter(needle, StringComparison.Ordinal));
        Assert.Equal(result, value.CutSpanAfter(needle, StringComparison.OrdinalIgnoreCase));

        Assert.False(value.TryCutSpanAfter(needle, StringComparison.Ordinal, out trySpanResult));
        Assert.Equal(ReadOnlySpan<char>.Empty, trySpanResult);

        Assert.True(value.TryCutSpanAfter(needle, StringComparison.OrdinalIgnoreCase, out trySpanResult));
        Assert.Equal(result, trySpanResult);

        // span for value
        // the needle only matches when ignoring case, so overloads without a comparison must not match
        Assert.Equal(value, value.AsSpan().CutAfter(needle));
        Assert.Equal(value, value.AsSpan().CutAfter(needle, pool));
        Assert.False(value.AsSpan().TryCutAfter(needle, out tryResult));
        Assert.False(value.AsSpan().TryCutAfter(needle, pool, out tryResult));
        Assert.False(value.AsSpan().TryCutAfter(needle, resultSpan, out tryWritten));
        Assert.Equal(value, value.AsSpan().CutSpanAfter(needle));
        Assert.False(value.AsSpan().TryCutSpanAfter(needle, out trySpanResult));

        Assert.Equal(value, value.AsSpan().CutAfter(needle, StringComparison.Ordinal));
        Assert.Equal(value, value.AsSpan().CutAfter(needle, StringComparison.Ordinal, pool));

        Assert.Equal(result, value.AsSpan().CutAfter(needle, StringComparison.OrdinalIgnoreCase));
        Assert.Same(pool.GetOrAdd(result), value.AsSpan().CutAfter(needle, StringComparison.OrdinalIgnoreCase, pool));

        Assert.False(value.AsSpan().TryCutAfter(needle, StringComparison.Ordinal, out tryResult));
        Assert.Null(tryResult);
        Assert.False(value.AsSpan().TryCutAfter(needle, StringComparison.Ordinal, pool, out tryResult));
        Assert.Null(tryResult);

        Assert.True(value.AsSpan().TryCutAfter(needle, StringComparison.OrdinalIgnoreCase, out tryResult));
        Assert.Equal(result, tryResult);
        Assert.True(value.AsSpan().TryCutAfter(needle, StringComparison.OrdinalIgnoreCase, pool, out tryResult));
        Assert.Same(pool.GetOrAdd(result), tryResult);

        resultSpan.Fill(resultSpanInitChar);
        Assert.False(value.AsSpan().TryCutAfter(needle, StringComparison.Ordinal, resultSpan, out tryWritten));
        Assert.Equal(resultSpanInit, resultSpan);
        Assert.Equal(0, tryWritten);

        resultSpan.Fill(resultSpanInitChar);
        Assert.True(value.AsSpan().TryCutAfter(needle, StringComparison.OrdinalIgnoreCase, resultSpan, out tryWritten));
        Assert.Equal(result, resultSpan);
        Assert.Equal(resultSpan.Length, tryWritten);

        smallResultSpan.Fill(resultSpanInitChar);
        Assert.False(value.AsSpan().TryCutAfter(needle, StringComparison.OrdinalIgnoreCase, smallResultSpan, out tryWritten));
        Assert.Equal(resultSpanInit[..smallResultSpan.Length], smallResultSpan);
        Assert.Equal(0, tryWritten);

        Assert.Equal(value, value.AsSpan().CutSpanAfter(needle, StringComparison.Ordinal));
        Assert.Equal(result, value.AsSpan().CutSpanAfter(needle, StringComparison.OrdinalIgnoreCase));

        Assert.False(value.AsSpan().TryCutSpanAfter(needle, StringComparison.Ordinal, out trySpanResult));
        Assert.Equal(ReadOnlySpan<char>.Empty, trySpanResult);

        Assert.True(value.AsSpan().TryCutSpanAfter(needle, StringComparison.OrdinalIgnoreCase, out trySpanResult));
        Assert.Equal(result, trySpanResult);
    }

    [Fact]
    public void CutAfterLast_String_Comparison()
    {
        const string needle = "AB";
        const string value = "123_ab_456_Ab_789";
        const string result = "_789";
        const char resultSpanInitChar = '#';

        Span<char> resultSpanInit = stackalloc char[result.Length];
        resultSpanInit.Fill(resultSpanInitChar);

        Span<char> resultSpan = stackalloc char[result.Length];
        Span<char> smallResultSpan = stackalloc char[result.Length - 1];
        StringPool pool = new();
        string? tryResult;
        ReadOnlySpan<char> trySpanResult;
        int tryWritten;

        // string for value
        // the needle only matches when ignoring case, so overloads without a comparison must not match
        Assert.Equal(value, value.CutAfterLast(needle));
        Assert.Equal(value, value.CutAfterLast(needle, pool));
        Assert.False(value.TryCutAfterLast(needle, out tryResult));
        Assert.False(value.TryCutAfterLast(needle, pool, out tryResult));
        Assert.False(value.TryCutAfterLast(needle, resultSpan, out tryWritten));
        Assert.Equal(value, value.CutSpanAfterLast(needle));
        Assert.False(value.TryCutSpanAfterLast(needle, out trySpanResult));

        Assert.Equal(value, value.CutAfterLast(needle, StringComparison.Ordinal));
        Assert.Equal(value, value.CutAfterLast(needle, StringComparison.Ordinal, pool));

        Assert.Equal(result, value.CutAfterLast(needle, StringComparison.OrdinalIgnoreCase));
        Assert.Same(pool.GetOrAdd(result), value.CutAfterLast(needle, StringComparison.OrdinalIgnoreCase, pool));

        Assert.False(value.TryCutAfterLast(needle, StringComparison.Ordinal, out tryResult));
        Assert.Null(tryResult);
        Assert.False(value.TryCutAfterLast(needle, StringComparison.Ordinal, pool, out tryResult));
        Assert.Null(tryResult);

        Assert.True(value.TryCutAfterLast(needle, StringComparison.OrdinalIgnoreCase, out tryResult));
        Assert.Equal(result, tryResult);
        Assert.True(value.TryCutAfterLast(needle, StringComparison.OrdinalIgnoreCase, pool, out tryResult));
        Assert.Same(pool.GetOrAdd(result), tryResult);

        resultSpan.Fill(resultSpanInitChar);
        Assert.False(value.TryCutAfterLast(needle, StringComparison.Ordinal, resultSpan, out tryWritten));
        Assert.Equal(resultSpanInit, resultSpan);
        Assert.Equal(0, tryWritten);

        resultSpan.Fill(resultSpanInitChar);
        Assert.True(value.TryCutAfterLast(needle, StringComparison.OrdinalIgnoreCase, resultSpan, out tryWritten));
        Assert.Equal(result, resultSpan);
        Assert.Equal(resultSpan.Length, tryWritten);

        smallResultSpan.Fill(resultSpanInitChar);
        Assert.False(value.TryCutAfterLast(needle, StringComparison.OrdinalIgnoreCase, smallResultSpan, out tryWritten));
        Assert.Equal(resultSpanInit[..smallResultSpan.Length], smallResultSpan);
        Assert.Equal(0, tryWritten);

        Assert.Equal(value, value.CutSpanAfterLast(needle, StringComparison.Ordinal));
        Assert.Equal(result, value.CutSpanAfterLast(needle, StringComparison.OrdinalIgnoreCase));

        Assert.False(value.TryCutSpanAfterLast(needle, StringComparison.Ordinal, out trySpanResult));
        Assert.Equal(ReadOnlySpan<char>.Empty, trySpanResult);

        Assert.True(value.TryCutSpanAfterLast(needle, StringComparison.OrdinalIgnoreCase, out trySpanResult));
        Assert.Equal(result, trySpanResult);

        // span for value
        // the needle only matches when ignoring case, so overloads without a comparison must not match
        Assert.Equal(value, value.AsSpan().CutAfterLast(needle));
        Assert.Equal(value, value.AsSpan().CutAfterLast(needle, pool));
        Assert.False(value.AsSpan().TryCutAfterLast(needle, out tryResult));
        Assert.False(value.AsSpan().TryCutAfterLast(needle, pool, out tryResult));
        Assert.False(value.AsSpan().TryCutAfterLast(needle, resultSpan, out tryWritten));
        Assert.Equal(value, value.AsSpan().CutSpanAfterLast(needle));
        Assert.False(value.AsSpan().TryCutSpanAfterLast(needle, out trySpanResult));

        Assert.Equal(value, value.AsSpan().CutAfterLast(needle, StringComparison.Ordinal));
        Assert.Equal(value, value.AsSpan().CutAfterLast(needle, StringComparison.Ordinal, pool));

        Assert.Equal(result, value.AsSpan().CutAfterLast(needle, StringComparison.OrdinalIgnoreCase));
        Assert.Same(pool.GetOrAdd(result), value.AsSpan().CutAfterLast(needle, StringComparison.OrdinalIgnoreCase, pool));

        Assert.False(value.AsSpan().TryCutAfterLast(needle, StringComparison.Ordinal, out tryResult));
        Assert.Null(tryResult);
        Assert.False(value.AsSpan().TryCutAfterLast(needle, StringComparison.Ordinal, pool, out tryResult));
        Assert.Null(tryResult);

        Assert.True(value.AsSpan().TryCutAfterLast(needle, StringComparison.OrdinalIgnoreCase, out tryResult));
        Assert.Equal(result, tryResult);
        Assert.True(value.AsSpan().TryCutAfterLast(needle, StringComparison.OrdinalIgnoreCase, pool, out tryResult));
        Assert.Same(pool.GetOrAdd(result), tryResult);

        resultSpan.Fill(resultSpanInitChar);
        Assert.False(value.AsSpan().TryCutAfterLast(needle, StringComparison.Ordinal, resultSpan, out tryWritten));
        Assert.Equal(resultSpanInit, resultSpan);
        Assert.Equal(0, tryWritten);

        resultSpan.Fill(resultSpanInitChar);
        Assert.True(value.AsSpan().TryCutAfterLast(needle, StringComparison.OrdinalIgnoreCase, resultSpan, out tryWritten));
        Assert.Equal(result, resultSpan);
        Assert.Equal(resultSpan.Length, tryWritten);

        smallResultSpan.Fill(resultSpanInitChar);
        Assert.False(value.AsSpan().TryCutAfterLast(needle, StringComparison.OrdinalIgnoreCase, smallResultSpan, out tryWritten));
        Assert.Equal(resultSpanInit[..smallResultSpan.Length], smallResultSpan);
        Assert.Equal(0, tryWritten);

        Assert.Equal(value, value.AsSpan().CutSpanAfterLast(needle, StringComparison.Ordinal));
        Assert.Equal(result, value.AsSpan().CutSpanAfterLast(needle, StringComparison.OrdinalIgnoreCase));

        Assert.False(value.AsSpan().TryCutSpanAfterLast(needle, StringComparison.Ordinal, out trySpanResult));
        Assert.Equal(ReadOnlySpan<char>.Empty, trySpanResult);

        Assert.True(value.AsSpan().TryCutSpanAfterLast(needle, StringComparison.OrdinalIgnoreCase, out trySpanResult));
        Assert.Equal(result, trySpanResult);
    }

    [Fact]
    public void CutBefore_String_Comparison()
    {
        const string needle = "AB";
        const string value = "123_ab_456_Ab_789";
        const string result = "123_";
        const char resultSpanInitChar = '#';

        Span<char> resultSpanInit = stackalloc char[result.Length];
        resultSpanInit.Fill(resultSpanInitChar);

        Span<char> resultSpan = stackalloc char[result.Length];
        Span<char> smallResultSpan = stackalloc char[result.Length - 1];
        StringPool pool = new();
        string? tryResult;
        ReadOnlySpan<char> trySpanResult;
        int tryWritten;

        // string for value
        // the needle only matches when ignoring case, so overloads without a comparison must not match
        Assert.Equal(value, value.CutBefore(needle));
        Assert.Equal(value, value.CutBefore(needle, pool));
        Assert.False(value.TryCutBefore(needle, out tryResult));
        Assert.False(value.TryCutBefore(needle, pool, out tryResult));
        Assert.False(value.TryCutBefore(needle, resultSpan, out tryWritten));
        Assert.Equal(value, value.CutSpanBefore(needle));
        Assert.False(value.TryCutSpanBefore(needle, out trySpanResult));

        Assert.Equal(value, value.CutBefore(needle, StringComparison.Ordinal));
        Assert.Equal(value, value.CutBefore(needle, StringComparison.Ordinal, pool));

        Assert.Equal(result, value.CutBefore(needle, StringComparison.OrdinalIgnoreCase));
        Assert.Same(pool.GetOrAdd(result), value.CutBefore(needle, StringComparison.OrdinalIgnoreCase, pool));

        Assert.False(value.TryCutBefore(needle, StringComparison.Ordinal, out tryResult));
        Assert.Null(tryResult);
        Assert.False(value.TryCutBefore(needle, StringComparison.Ordinal, pool, out tryResult));
        Assert.Null(tryResult);

        Assert.True(value.TryCutBefore(needle, StringComparison.OrdinalIgnoreCase, out tryResult));
        Assert.Equal(result, tryResult);
        Assert.True(value.TryCutBefore(needle, StringComparison.OrdinalIgnoreCase, pool, out tryResult));
        Assert.Same(pool.GetOrAdd(result), tryResult);

        resultSpan.Fill(resultSpanInitChar);
        Assert.False(value.TryCutBefore(needle, StringComparison.Ordinal, resultSpan, out tryWritten));
        Assert.Equal(resultSpanInit, resultSpan);
        Assert.Equal(0, tryWritten);

        resultSpan.Fill(resultSpanInitChar);
        Assert.True(value.TryCutBefore(needle, StringComparison.OrdinalIgnoreCase, resultSpan, out tryWritten));
        Assert.Equal(result, resultSpan);
        Assert.Equal(resultSpan.Length, tryWritten);

        smallResultSpan.Fill(resultSpanInitChar);
        Assert.False(value.TryCutBefore(needle, StringComparison.OrdinalIgnoreCase, smallResultSpan, out tryWritten));
        Assert.Equal(resultSpanInit[..smallResultSpan.Length], smallResultSpan);
        Assert.Equal(0, tryWritten);

        Assert.Equal(value, value.CutSpanBefore(needle, StringComparison.Ordinal));
        Assert.Equal(result, value.CutSpanBefore(needle, StringComparison.OrdinalIgnoreCase));

        Assert.False(value.TryCutSpanBefore(needle, StringComparison.Ordinal, out trySpanResult));
        Assert.Equal(ReadOnlySpan<char>.Empty, trySpanResult);

        Assert.True(value.TryCutSpanBefore(needle, StringComparison.OrdinalIgnoreCase, out trySpanResult));
        Assert.Equal(result, trySpanResult);

        // span for value
        // the needle only matches when ignoring case, so overloads without a comparison must not match
        Assert.Equal(value, value.AsSpan().CutBefore(needle));
        Assert.Equal(value, value.AsSpan().CutBefore(needle, pool));
        Assert.False(value.AsSpan().TryCutBefore(needle, out tryResult));
        Assert.False(value.AsSpan().TryCutBefore(needle, pool, out tryResult));
        Assert.False(value.AsSpan().TryCutBefore(needle, resultSpan, out tryWritten));
        Assert.Equal(value, value.AsSpan().CutSpanBefore(needle));
        Assert.False(value.AsSpan().TryCutSpanBefore(needle, out trySpanResult));

        Assert.Equal(value, value.AsSpan().CutBefore(needle, StringComparison.Ordinal));
        Assert.Equal(value, value.AsSpan().CutBefore(needle, StringComparison.Ordinal, pool));

        Assert.Equal(result, value.AsSpan().CutBefore(needle, StringComparison.OrdinalIgnoreCase));
        Assert.Same(pool.GetOrAdd(result), value.AsSpan().CutBefore(needle, StringComparison.OrdinalIgnoreCase, pool));

        Assert.False(value.AsSpan().TryCutBefore(needle, StringComparison.Ordinal, out tryResult));
        Assert.Null(tryResult);
        Assert.False(value.AsSpan().TryCutBefore(needle, StringComparison.Ordinal, pool, out tryResult));
        Assert.Null(tryResult);

        Assert.True(value.AsSpan().TryCutBefore(needle, StringComparison.OrdinalIgnoreCase, out tryResult));
        Assert.Equal(result, tryResult);
        Assert.True(value.AsSpan().TryCutBefore(needle, StringComparison.OrdinalIgnoreCase, pool, out tryResult));
        Assert.Same(pool.GetOrAdd(result), tryResult);

        resultSpan.Fill(resultSpanInitChar);
        Assert.False(value.AsSpan().TryCutBefore(needle, StringComparison.Ordinal, resultSpan, out tryWritten));
        Assert.Equal(resultSpanInit, resultSpan);
        Assert.Equal(0, tryWritten);

        resultSpan.Fill(resultSpanInitChar);
        Assert.True(value.AsSpan().TryCutBefore(needle, StringComparison.OrdinalIgnoreCase, resultSpan, out tryWritten));
        Assert.Equal(result, resultSpan);
        Assert.Equal(resultSpan.Length, tryWritten);

        smallResultSpan.Fill(resultSpanInitChar);
        Assert.False(value.AsSpan().TryCutBefore(needle, StringComparison.OrdinalIgnoreCase, smallResultSpan, out tryWritten));
        Assert.Equal(resultSpanInit[..smallResultSpan.Length], smallResultSpan);
        Assert.Equal(0, tryWritten);

        Assert.Equal(value, value.AsSpan().CutSpanBefore(needle, StringComparison.Ordinal));
        Assert.Equal(result, value.AsSpan().CutSpanBefore(needle, StringComparison.OrdinalIgnoreCase));

        Assert.False(value.AsSpan().TryCutSpanBefore(needle, StringComparison.Ordinal, out trySpanResult));
        Assert.Equal(ReadOnlySpan<char>.Empty, trySpanResult);

        Assert.True(value.AsSpan().TryCutSpanBefore(needle, StringComparison.OrdinalIgnoreCase, out trySpanResult));
        Assert.Equal(result, trySpanResult);
    }

    [Fact]
    public void CutBeforeLast_String_Comparison()
    {
        const string needle = "AB";
        const string value = "123_ab_456_Ab_789";
        const string result = "123_ab_456_";
        const char resultSpanInitChar = '#';

        Span<char> resultSpanInit = stackalloc char[result.Length];
        resultSpanInit.Fill(resultSpanInitChar);

        Span<char> resultSpan = stackalloc char[result.Length];
        Span<char> smallResultSpan = stackalloc char[result.Length - 1];
        StringPool pool = new();
        string? tryResult;
        ReadOnlySpan<char> trySpanResult;
        int tryWritten;

        // string for value
        // the needle only matches when ignoring case, so overloads without a comparison must not match
        Assert.Equal(value, value.CutBeforeLast(needle));
        Assert.Equal(value, value.CutBeforeLast(needle, pool));
        Assert.False(value.TryCutBeforeLast(needle, out tryResult));
        Assert.False(value.TryCutBeforeLast(needle, pool, out tryResult));
        Assert.False(value.TryCutBeforeLast(needle, resultSpan, out tryWritten));
        Assert.Equal(value, value.CutSpanBeforeLast(needle));
        Assert.False(value.TryCutSpanBeforeLast(needle, out trySpanResult));

        Assert.Equal(value, value.CutBeforeLast(needle, StringComparison.Ordinal));
        Assert.Equal(value, value.CutBeforeLast(needle, StringComparison.Ordinal, pool));

        Assert.Equal(result, value.CutBeforeLast(needle, StringComparison.OrdinalIgnoreCase));
        Assert.Same(pool.GetOrAdd(result), value.CutBeforeLast(needle, StringComparison.OrdinalIgnoreCase, pool));

        Assert.False(value.TryCutBeforeLast(needle, StringComparison.Ordinal, out tryResult));
        Assert.Null(tryResult);
        Assert.False(value.TryCutBeforeLast(needle, StringComparison.Ordinal, pool, out tryResult));
        Assert.Null(tryResult);

        Assert.True(value.TryCutBeforeLast(needle, StringComparison.OrdinalIgnoreCase, out tryResult));
        Assert.Equal(result, tryResult);
        Assert.True(value.TryCutBeforeLast(needle, StringComparison.OrdinalIgnoreCase, pool, out tryResult));
        Assert.Same(pool.GetOrAdd(result), tryResult);

        resultSpan.Fill(resultSpanInitChar);
        Assert.False(value.TryCutBeforeLast(needle, StringComparison.Ordinal, resultSpan, out tryWritten));
        Assert.Equal(resultSpanInit, resultSpan);
        Assert.Equal(0, tryWritten);

        resultSpan.Fill(resultSpanInitChar);
        Assert.True(value.TryCutBeforeLast(needle, StringComparison.OrdinalIgnoreCase, resultSpan, out tryWritten));
        Assert.Equal(result, resultSpan);
        Assert.Equal(resultSpan.Length, tryWritten);

        smallResultSpan.Fill(resultSpanInitChar);
        Assert.False(value.TryCutBeforeLast(needle, StringComparison.OrdinalIgnoreCase, smallResultSpan, out tryWritten));
        Assert.Equal(resultSpanInit[..smallResultSpan.Length], smallResultSpan);
        Assert.Equal(0, tryWritten);

        Assert.Equal(value, value.CutSpanBeforeLast(needle, StringComparison.Ordinal));
        Assert.Equal(result, value.CutSpanBeforeLast(needle, StringComparison.OrdinalIgnoreCase));

        Assert.False(value.TryCutSpanBeforeLast(needle, StringComparison.Ordinal, out trySpanResult));
        Assert.Equal(ReadOnlySpan<char>.Empty, trySpanResult);

        Assert.True(value.TryCutSpanBeforeLast(needle, StringComparison.OrdinalIgnoreCase, out trySpanResult));
        Assert.Equal(result, trySpanResult);

        // span for value
        // the needle only matches when ignoring case, so overloads without a comparison must not match
        Assert.Equal(value, value.AsSpan().CutBeforeLast(needle));
        Assert.Equal(value, value.AsSpan().CutBeforeLast(needle, pool));
        Assert.False(value.AsSpan().TryCutBeforeLast(needle, out tryResult));
        Assert.False(value.AsSpan().TryCutBeforeLast(needle, pool, out tryResult));
        Assert.False(value.AsSpan().TryCutBeforeLast(needle, resultSpan, out tryWritten));
        Assert.Equal(value, value.AsSpan().CutSpanBeforeLast(needle));
        Assert.False(value.AsSpan().TryCutSpanBeforeLast(needle, out trySpanResult));

        Assert.Equal(value, value.AsSpan().CutBeforeLast(needle, StringComparison.Ordinal));
        Assert.Equal(value, value.AsSpan().CutBeforeLast(needle, StringComparison.Ordinal, pool));

        Assert.Equal(result, value.AsSpan().CutBeforeLast(needle, StringComparison.OrdinalIgnoreCase));
        Assert.Same(pool.GetOrAdd(result), value.AsSpan().CutBeforeLast(needle, StringComparison.OrdinalIgnoreCase, pool));

        Assert.False(value.AsSpan().TryCutBeforeLast(needle, StringComparison.Ordinal, out tryResult));
        Assert.Null(tryResult);
        Assert.False(value.AsSpan().TryCutBeforeLast(needle, StringComparison.Ordinal, pool, out tryResult));
        Assert.Null(tryResult);

        Assert.True(value.AsSpan().TryCutBeforeLast(needle, StringComparison.OrdinalIgnoreCase, out tryResult));
        Assert.Equal(result, tryResult);
        Assert.True(value.AsSpan().TryCutBeforeLast(needle, StringComparison.OrdinalIgnoreCase, pool, out tryResult));
        Assert.Same(pool.GetOrAdd(result), tryResult);

        resultSpan.Fill(resultSpanInitChar);
        Assert.False(value.AsSpan().TryCutBeforeLast(needle, StringComparison.Ordinal, resultSpan, out tryWritten));
        Assert.Equal(resultSpanInit, resultSpan);
        Assert.Equal(0, tryWritten);

        resultSpan.Fill(resultSpanInitChar);
        Assert.True(value.AsSpan().TryCutBeforeLast(needle, StringComparison.OrdinalIgnoreCase, resultSpan, out tryWritten));
        Assert.Equal(result, resultSpan);
        Assert.Equal(resultSpan.Length, tryWritten);

        smallResultSpan.Fill(resultSpanInitChar);
        Assert.False(value.AsSpan().TryCutBeforeLast(needle, StringComparison.OrdinalIgnoreCase, smallResultSpan, out tryWritten));
        Assert.Equal(resultSpanInit[..smallResultSpan.Length], smallResultSpan);
        Assert.Equal(0, tryWritten);

        Assert.Equal(value, value.AsSpan().CutSpanBeforeLast(needle, StringComparison.Ordinal));
        Assert.Equal(result, value.AsSpan().CutSpanBeforeLast(needle, StringComparison.OrdinalIgnoreCase));

        Assert.False(value.AsSpan().TryCutSpanBeforeLast(needle, StringComparison.Ordinal, out trySpanResult));
        Assert.Equal(ReadOnlySpan<char>.Empty, trySpanResult);

        Assert.True(value.AsSpan().TryCutSpanBeforeLast(needle, StringComparison.OrdinalIgnoreCase, out trySpanResult));
        Assert.Equal(result, trySpanResult);
    }

    [Theory]
    [InlineData(StringComparison.CurrentCulture)]
    [InlineData(StringComparison.CurrentCultureIgnoreCase)]
    [InlineData(StringComparison.InvariantCulture)]
    [InlineData(StringComparison.InvariantCultureIgnoreCase)]
    public void Cut_String_CultureMatchLength(StringComparison comparisonType)
    {
        // with a culture-sensitive comparison the matched text can be longer or shorter than the needle
        const string precomposed = "\u00C5"; // Å
        const string decomposed = "A\u030A"; // A + combining ring above, canonically equivalent to Å
        const string softHyphen = "\u00AD"; // ignored by culture-sensitive comparisons

        Span<char> resultSpan = stackalloc char[16];
        string? tryResult;
        ReadOnlySpan<char> trySpanResult;
        int tryWritten;

        // the match is longer than the needle
        const string decomposedValue = "1" + decomposed + "2" + decomposed + "3";
        Assert.Equal("2" + decomposed + "3", decomposedValue.CutAfter(precomposed, comparisonType));
        Assert.Equal("3", decomposedValue.CutAfterLast(precomposed, comparisonType));
        Assert.Equal("1", decomposedValue.CutBefore(precomposed, comparisonType));
        Assert.Equal("1" + decomposed + "2", decomposedValue.CutBeforeLast(precomposed, comparisonType));
        Assert.Equal("2" + decomposed + "3", decomposedValue.AsSpan().CutSpanAfter(precomposed, comparisonType));
        Assert.Equal("3", decomposedValue.AsSpan().CutSpanAfterLast(precomposed, comparisonType));

        const string softHyphenValue = "1b" + softHyphen + "c2b" + softHyphen + "c3";
        Assert.Equal("2b" + softHyphen + "c3", softHyphenValue.CutAfter("bc", comparisonType));
        Assert.Equal("3", softHyphenValue.CutAfterLast("bc", comparisonType));
        Assert.Equal("1", softHyphenValue.CutBefore("bc", comparisonType));
        Assert.Equal("1b" + softHyphen + "c2", softHyphenValue.CutBeforeLast("bc", comparisonType));
        Assert.Equal("2b" + softHyphen + "c3", softHyphenValue.AsSpan().CutSpanAfter("bc", comparisonType));
        Assert.Equal("3", softHyphenValue.AsSpan().CutSpanAfterLast("bc", comparisonType));

        // the match is shorter than the needle
        const string precomposedValue = "1" + precomposed + "2" + precomposed + "3";
        Assert.Equal("2" + precomposed + "3", precomposedValue.CutAfter(decomposed, comparisonType));
        Assert.Equal("3", precomposedValue.CutAfterLast(decomposed, comparisonType));
        Assert.Equal("1", precomposedValue.CutBefore(decomposed, comparisonType));
        Assert.Equal("1" + precomposed + "2", precomposedValue.CutBeforeLast(decomposed, comparisonType));
        Assert.Equal("2" + precomposed + "3", precomposedValue.AsSpan().CutSpanAfter(decomposed, comparisonType));
        Assert.Equal("3", precomposedValue.AsSpan().CutSpanAfterLast(decomposed, comparisonType));

        const string softHyphenNeedle = "b" + softHyphen + "c";
        Assert.Equal("2bc3", "1bc2bc3".CutAfter(softHyphenNeedle, comparisonType));
        Assert.Equal("3", "1bc2bc3".CutAfterLast(softHyphenNeedle, comparisonType));
        Assert.Equal("1", "1bc2bc3".CutBefore(softHyphenNeedle, comparisonType));
        Assert.Equal("1bc2", "1bc2bc3".CutBeforeLast(softHyphenNeedle, comparisonType));
        Assert.Equal("2bc3", "1bc2bc3".AsSpan().CutSpanAfter(softHyphenNeedle, comparisonType));
        Assert.Equal("3", "1bc2bc3".AsSpan().CutSpanAfterLast(softHyphenNeedle, comparisonType));

        // a match at the end of the value that is shorter than the needle
        const string precomposedEndValue = "1" + precomposed;
        Assert.True(precomposedEndValue.TryCutAfter(decomposed, comparisonType, out tryResult));
        Assert.Equal("", tryResult);
        Assert.True(precomposedEndValue.TryCutAfterLast(decomposed, comparisonType, out tryResult));
        Assert.Equal("", tryResult);
        Assert.True(precomposedEndValue.AsSpan().TryCutSpanAfter(decomposed, comparisonType, out trySpanResult));
        Assert.Equal("", trySpanResult);
        Assert.True(precomposedEndValue.AsSpan().TryCutSpanAfterLast(decomposed, comparisonType, out trySpanResult));
        Assert.Equal("", trySpanResult);

        // other result shapes
        Assert.True(decomposedValue.AsSpan().TryCutAfter(precomposed, comparisonType, StringPool.Shared, out tryResult));
        Assert.Equal("2" + decomposed + "3", tryResult);
        Assert.True(decomposedValue.TryCutAfter(precomposed, comparisonType, resultSpan, out tryWritten));
        Assert.Equal("2" + decomposed + "3", resultSpan[..tryWritten]);
        Assert.True(precomposedValue.AsSpan().TryCutAfterLast(decomposed, comparisonType, resultSpan, out tryWritten));
        Assert.Equal("3", resultSpan[..tryWritten]);

        // a needle made only of ignorable characters matches like an empty needle
        Assert.True("abc".TryCutAfter(softHyphen, comparisonType, out tryResult));
        Assert.Equal("abc", tryResult);
        Assert.True("abc".TryCutAfterLast(softHyphen, comparisonType, out tryResult));
        Assert.Equal("", tryResult);
        Assert.True("abc".TryCutBefore(softHyphen, comparisonType, out tryResult));
        Assert.Equal("", tryResult);
        Assert.True("abc".TryCutBeforeLast(softHyphen, comparisonType, out tryResult));
        Assert.Equal("abc", tryResult);
    }

    [Fact]
    public void Cut_String_CurrentCulture()
    {
        // in Turkish, "I" is the upper case of the dotless "ı" and not of "i"
        const string needle = "I";
        const string dotlessI = "\u0131"; // ı
        const string value = "1i2" + dotlessI + "3";

        CultureInfo culture = CultureInfo.CurrentCulture;
        try
        {
            CultureInfo.CurrentCulture = CultureInfo.GetCultureInfo("tr-TR");

            Assert.False(value.TryCutAfter(needle, StringComparison.CurrentCulture, out _));
            Assert.False(value.TryCutAfter(needle, StringComparison.InvariantCulture, out _));

            Assert.Equal("3", value.CutAfter(needle, StringComparison.CurrentCultureIgnoreCase));
            Assert.Equal("3", value.CutAfterLast(needle, StringComparison.CurrentCultureIgnoreCase));
            Assert.Equal("1i2", value.CutBefore(needle, StringComparison.CurrentCultureIgnoreCase));
            Assert.Equal("1i2", value.CutBeforeLast(needle, StringComparison.CurrentCultureIgnoreCase));

            Assert.Equal("2" + dotlessI + "3", value.CutAfter(needle, StringComparison.InvariantCultureIgnoreCase));
            Assert.Equal("2" + dotlessI + "3", value.CutAfterLast(needle, StringComparison.InvariantCultureIgnoreCase));
            Assert.Equal("1", value.CutBefore(needle, StringComparison.InvariantCultureIgnoreCase));
            Assert.Equal("1", value.CutBeforeLast(needle, StringComparison.InvariantCultureIgnoreCase));
        }
        finally
        {
            CultureInfo.CurrentCulture = culture;
        }
    }
}
