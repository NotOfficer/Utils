using System.Runtime.CompilerServices;

namespace OffiUtils;

public static class DateTimeUtils
{
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static DateTime Trim(this DateTime date, long roundTicks)
    {
        return new DateTime(date.Ticks - date.Ticks % roundTicks, date.Kind);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static DateTimeOffset Trim(this DateTimeOffset date, long roundTicks)
    {
        return new DateTimeOffset(date.Ticks - date.Ticks % roundTicks, date.Offset);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static TimeOnly Trim(this TimeOnly time, long roundTicks)
    {
        return new TimeOnly(time.Ticks - time.Ticks % roundTicks);
    }
}
