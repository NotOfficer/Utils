namespace OffiUtils;

public static class CollectionUtils
{
	public static Dictionary<TKey, TValue> FilterBy<TKey, TValue>(this IReadOnlyDictionary<TKey, TValue> source, Predicate<KeyValuePair<TKey, TValue>> predicate)
		where TKey : notnull
	{
		var result = new Dictionary<TKey, TValue>();

		foreach (var item in source)
		{
			if (predicate(item))
			{
				result.Add(item.Key, item.Value);
			}
		}

		return result;
	}
}
