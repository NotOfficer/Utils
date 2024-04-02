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

	public static Span<T> AsSpan<T>(ReadOnlySpan<T> readonlySpan)
	{
		if (readonlySpan.IsEmpty) return Span<T>.Empty;
		ref var reference = ref MemoryMarshal.GetReference(readonlySpan);
		return MemoryMarshal.CreateSpan(ref reference, readonlySpan.Length);
	}

	public static void FreeGCHandleIfValid(ref nint ptr)
	{
		if (ptr == IntPtr.Zero) return;
		var gch = GCHandle.FromIntPtr(ptr);
		gch.Free();
		ptr = nint.Zero;
	}

	public static void FreeLibraryIfValid(ref nint handle)
	{
		if (handle == IntPtr.Zero) return;
		NativeLibrary.Free(handle);
		handle = nint.Zero;
	}
}
