import msgpack

ASSERT_TEMPLATE = """
(test-group "%(name)s"
(test "unpack" %(chicken_expr)s (unpack/from-blob (byte-blob %(blob)s)))
(test "pack" (pack/to-blob %(chicken_expr)s) (byte-blob %(blob)s)))
"""

def asChickenAssertion(data, chicken_expr=None, name=None):
    blob = ' '.join([hex(b).replace('0', '#', 1) for b in msgpack.dumps(data)])
    return ASSERT_TEMPLATE % {
        "name": name if name else str(data),
        "blob": blob,
        "chicken_expr": data if chicken_expr is None else chicken_expr
    }

header = open("tests/python-ref-header.scm").readlines()

test_ref_file = open("tests/python-ref-tests.scm", "w")
test_ref_file.writelines(header)

def append_assert(*args, **kwargs):
    test_ref_file.write(asChickenAssertion(*args, **kwargs))

append_assert( -1)
append_assert(-100)
append_assert(100)
append_assert(100102831903)
append_assert(-100102831903)
append_assert(1.3313)
append_assert([], "'#()")
append_assert([10, True, ["hi"]], """'#(10 #t #("hi"))""")
append_assert(
    msgpack.ExtType(42, 'a'.encode('utf8')),
    """(make-extension 42 (string->byte-blob "a"))""")
