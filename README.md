MessagePack for Scheme
======================

An implementation of [MessagePack](http://msgpack.org/) for Scheme.

API Specification
-----------------

Primitive pack-family procedures:

```scheme
(pack-uint PORT value)
(pack-sint PORT value)
(pack-float PORT FLONUM)
(pack-double PORT FLONUM)
(pack-raw PORT BYTE-BLOB)  ; byte-blob
(pack-array PORT VECTOR)   ; vector
(pack-map PORT HASH-TABLE) ; hash-table
```

Additionally, this implementation provides a generic pack procedure:

```scheme
(pack PORT value)
```

This procedure will call primitive type packers, with the following rules:

- if the value has a packer, apply it.
- if the value is a string, it will be packed as raw.
- if the value is a blob, it will be packed as raw.
- if the value is a char, it will be packed as a uint.
- if the value is a list, it will be packed as an array.

Unpack procedures:

```scheme
(unpack PORT [mapper])
```

Mappers:

```scheme
(raw->string/mapper value)
```

License
-------

Distributed under the New BSD License.
