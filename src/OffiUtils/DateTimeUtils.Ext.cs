using System.Runtime.CompilerServices;

namespace OffiUtils;

public static class DateTimeUtils
{
    extension(DateTime date)
    {
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public DateTime Trim(long roundTicks)
        {
            return new DateTime(date.Ticks - date.Ticks % roundTicks, date.Kind);
        }
    }

    extension(DateTimeOffset date)
    {
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public DateTimeOffset Trim(long roundTicks)
        {
            return new DateTimeOffset(date.Ticks - date.Ticks % roundTicks, date.Offset);
        }
    }

    extension(TimeOnly time)
    {
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public TimeOnly Trim(long roundTicks)
        {
            return new TimeOnly(time.Ticks - time.Ticks % roundTicks);
        }
    }
}
