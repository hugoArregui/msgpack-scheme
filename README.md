MessagePack implementation for CHICKEN scheme
=============================================

An implementation of [MessagePack](http://msgpack.org/) for [CHICKEN scheme](https://www.call-cc.org/).

API Specification
-----------------

Primitive pack-family procedures:

```scheme
(pack-uint PORT value)
(pack-sint PORT value)
(pack-float PORT FLONUM)
(pack-double PORT FLONUM)
(pack-bin PORT BYTE-BLOB)  ; byte-blob
(pack-str PORT STRING)     ; string
(pack-array PORT VECTOR)   ; vector
(pack-map PORT HASH-TABLE) ; hash-table
(pack-ext PORT EXT)        ; extension (see below)
```

Additionally, this implementation provides a generic pack procedure:

```scheme
(pack PORT value)
```

This procedure will call primitive type packers, with the following rules:

- if the value has a packer, apply it.
- if the value is a string, it will be packed as str.
- if the value is a blob, it will be packed as bin.
- if the value is a char, it will be packed as a uint.
- if the value is a list, it will be packed as an array.
- if the value is a extension (see below), it will be packed as an ext

Unpack procedures:

```scheme
(unpack PORT [mapper])
```

Extension
---------

A extension type is simply a three element list (header type data):

- header: 'ext symbol
- type: integer from 0 to 127
- data: a byte-blob

```
(list 'ext TYPE DATA)
```

Example:


```scheme
(list 'ext 1 ,(string->byte-blob "hi"))
```


License
-------

Distributed under the New BSD License.
