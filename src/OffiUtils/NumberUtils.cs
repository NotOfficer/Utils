using System.Numerics;
using System.Runtime.CompilerServices;

namespace OffiUtils;

public static class NumberUtils
{
	[MethodImpl(MethodImplOptions.AggressiveInlining)]
	public static TVal Align<TVal>(TVal val, TVal alignment)
		where TVal : IBinaryInteger<TVal>
	{
		return val + alignment - TVal.One & ~(alignment - TVal.One);
	}

	[MethodImpl(MethodImplOptions.AggressiveInlining)]
	public static TVal Align<TVal, TAl>(TVal val, TAl alignment)
		where TVal : IBinaryInteger<TVal>
		where TAl : IBinaryInteger<TAl>
	{
		var al = TVal.CreateChecked(alignment);
		return val + al - TVal.One & ~(al - TVal.One);
	}

	public static ulong IntelOrder64(ulong value)
	{
		value = value << 8 & 0xFF00FF00FF00FF00UL | value >> 8 & 0x00FF00FF00FF00FFUL;
		value = value << 16 & 0xFFFF0000FFFF0000UL | value >> 16 & 0x0000FFFF0000FFFFUL;
		return value << 32 | value >> 32;
	}

	[MethodImpl(MethodImplOptions.AggressiveInlining)]
	public static TVal DivideAndRoundUp<TVal>(TVal dividend, TVal divisor)
		where TVal : IBinaryInteger<TVal>
	{
		return (dividend + divisor - TVal.One) / divisor;
	}

	[MethodImpl(MethodImplOptions.AggressiveInlining)]
	public static bool IsPowerOfTwo<TVal>(TVal val)
		where TVal : IBinaryInteger<TVal>
	{
		return (val & val - TVal.One) == TVal.Zero;
	}
}
