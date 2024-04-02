using System.Runtime.CompilerServices;

namespace OffiUtils;

public static class DateTimeUtils
{
	[MethodImpl(MethodImplOptions.AggressiveInlining)]
	public static DateTime Trim(this DateTime date, long roundTicks)
	{
		return new DateTime(date.Ticks - date.Ticks % roundTicks, date.Kind);
	}
}
