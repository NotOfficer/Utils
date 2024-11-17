namespace OffiUtils;

public static class CollectionUtils
{
	public static Dictionary<TKey, TValue> FilterBy<TKey, TValue>(
		this IReadOnlyDictionary<TKey, TValue> source,
		Predicate<KeyValuePair<TKey, TValue>> predicate,
		IEqualityComparer<TKey>? comparer)
		where TKey : notnull
	{
		var result = new Dictionary<TKey, TValue>(comparer);

		foreach (var item in source)
		{
			if (predicate(item))
			{
				result.Add(item.Key, item.Value);
			}
		}

		return result;
	}

	public static Dictionary<TKey, TValue> FilterByKey<TKey, TValue>(
		this IReadOnlyDictionary<TKey, TValue> source,
		Predicate<TKey> predicate,
		IEqualityComparer<TKey>? comparer)
		where TKey : notnull
	{
		var result = new Dictionary<TKey, TValue>(comparer);

		foreach (var item in source)
		{
			if (predicate(item.Key))
			{
				result.Add(item.Key, item.Value);
			}
		}

		return result;
	}

	public static Dictionary<TKey, TValue> FilterByValue<TKey, TValue>(
		this IReadOnlyDictionary<TKey, TValue> source,
		Predicate<TValue> predicate,
		IEqualityComparer<TKey>? comparer)
		where TKey : notnull
	{
		var result = new Dictionary<TKey, TValue>(comparer);

		foreach (var item in source)
		{
			if (predicate(item.Value))
			{
				result.Add(item.Key, item.Value);
			}
		}

		return result;
	}
}
