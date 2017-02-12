## About
This is a simple wrapper around database records. It gives you access to a single record in a collection through an object instance.

## How To
You can retrieve records similar to the direct database interface using `get` and `get-one`. If you need to insert a new record, you can create an empty instance for a collection using `hull`. You can then `save`, `insert`, and `delete` the record. Accessing the fields of the record works with `field`, with a special `id` reader for the `_id` field. You can list the known fields with `fields`. Finally, there's some convenience macros called `with-model-fields`, `with-model`, `with-model-save`, and `do-models`.
