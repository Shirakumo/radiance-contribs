## About
This is an extension/interfacing library for using the [Clip](https://shinmera.github.io/clip) templating system with Radiance. Particularly, it adds the following functionality:

* An option called `:clip` for `radiance:page`, `admin:panel`, and `profile:panel`. It takes the name of a template in the modules `template` directory. If given, it will parse the document when the page is called, and store it in the `*document*` variable. If the return value of the page's body is a `plump:node` it sets the content-type of the response to `"application/xhtml+xml; charset=utf-8"` and returns the serialised version of the node instead.
* The `time` lQuery function which will appropriately modify the node it receives as expected for a `<time>` tag. The argument it expects can be a local-time timestamp, a unibersal-time, or a local-time timestring. 
* Clip attributes `@href`, `@src`, `@link`, `@action`, and `@formaction` that translate special URIs into external URLs that will be stuffed in the corresponding attributes without the `@`. The original attribute is removed in turn.

Special URIs are like regular URIs with some exceptions:

* The URI can be followed by a number of arguments, which are each parsed and evaluated just like other values in a clip attribute.
* The URI can contain sequences like `{n}` where `n` is an integer. This sequence will be replaced by the value of the `n`th argument.
* The URI can, unlike standard URIs, contain a query and fragment part.
* Backslash escaping works.
* You can also reference Radiance resources. Resources are denoted as follows `<module resource-type arg*>`, where args are resolved as strings, or `{n}` as described above.

This allows you to very easily produce correct external URIs within your templates.
