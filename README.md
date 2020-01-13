# UniqueList

Create ordered lists with unique elements.

`UniqueList` is similar to `Set`, but with two major differences:

+ The underlying type of a `UniqueList` need not be `comparable`.
+ Elements can be ordered, like a regular list.

## Notes

UniqueList works using equality checks internally. That means your code will break if you use a type that cannot be equated using `==`. This includes functions and json (`Json.Encode.Value` and `Json.Decode.Value`).

## Contributing

If you think this package could be improved, let me know by opening
an [issue](https://github.com/Chadtech/unique-list/issues/new)
or a [pull request](https://github.com/Chadtech/unique-list).