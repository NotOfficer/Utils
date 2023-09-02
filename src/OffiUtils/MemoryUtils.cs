using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace OffiUtils;

public class MemoryUtils
{
	public static unsafe Span<T> AllocNative<T>(int length, out nint intPtr) where T : struct
	{
		var ptr = NativeMemory.Alloc((nuint)length, (nuint)Unsafe.SizeOf<T>());
		intPtr = new nint(ptr);
		return new Span<T>(ptr, length);
	}

	public static unsafe Span<T> AllocNativeZeroed<T>(int length, out nint intPtr) where T : struct
	{
		var ptr = NativeMemory.AllocZeroed((nuint)length, (nuint)Unsafe.SizeOf<T>());
		intPtr = new nint(ptr);
		return new Span<T>(ptr, length);
	}

	public static unsafe void FreeNative(nint intPtr) => NativeMemory.Free(intPtr.ToPointer());

	public static unsafe Span<T> GetSpan<T>(nint intPtr, int length) where T : struct => new(intPtr.ToPointer(), length);
}