namespace OffiUtils;

public static class CollectionUtils
{
    extension<TKey, TValue>(IReadOnlyDictionary<TKey, TValue> source) where TKey : notnull
    {
        public Dictionary<TKey, TValue> FilterBy(Predicate<KeyValuePair<TKey, TValue>> predicate,
            IEqualityComparer<TKey>? comparer)
        {
            var result = new Dictionary<TKey, TValue>(comparer);

            foreach (KeyValuePair<TKey, TValue> item in source)
            {
                if (predicate(item))
                {
                    result.Add(item.Key, item.Value);
                }
            }

            return result;
        }

        public Dictionary<TKey, TValue> FilterByKey(Predicate<TKey> predicate,
            IEqualityComparer<TKey>? comparer)
        {
            var result = new Dictionary<TKey, TValue>(comparer);

            foreach (KeyValuePair<TKey, TValue> item in source)
            {
                if (predicate(item.Key))
                {
                    result.Add(item.Key, item.Value);
                }
            }

            return result;
        }

        public Dictionary<TKey, TValue> FilterByValue(Predicate<TValue> predicate,
            IEqualityComparer<TKey>? comparer)
        {
            var result = new Dictionary<TKey, TValue>(comparer);

            foreach (KeyValuePair<TKey, TValue> item in source)
            {
                if (predicate(item.Value))
                {
                    result.Add(item.Key, item.Value);
                }
            }

            return result;
        }
    }
}
