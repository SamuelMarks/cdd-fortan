!> @file cdd_json_dom.f90
!> @brief Arena-allocated, insertion-order-preserving JSON DOM with flat string pooling.

!> @brief Strongly-typed JSON value nodes, object key/value tables, and DOM query/builder routines.
module cdd_json_dom
  use cdd_kinds, only: i32, i64, r64, lk, c_k
  use cdd_errors, only: cdd_error_t, cdd_error_clear, cdd_error_set_simple, &
    CDD_ERR_BUFFER_OVERFLOW, CDD_ERR_JSON_PARSE, CDD_ERR_INVALID_OPTION
  use cdd_string, only: cdd_buffer_t, buf_init, buf_append_str, str_eq
  implicit none (type, external)
  private

  public :: CDD_MAX_JNODES, CDD_MAX_JPROPS, CDD_MAX_JITEMS
  public :: JVAL_NULL, JVAL_BOOL, JVAL_INT, JVAL_FLOAT, JVAL_STRING, JVAL_ARRAY, JVAL_OBJECT
  public :: cdd_json_node_t, cdd_json_prop_t, cdd_json_item_t, cdd_json_doc_t
  public :: jdoc_init, jdoc_init_bounded
  public :: jdoc_add_null, jdoc_add_bool, jdoc_add_int, jdoc_add_float, jdoc_add_string
  public :: jdoc_add_array, jdoc_add_object
  public :: jdoc_array_append, jdoc_object_set
  public :: jdoc_get_key, jdoc_has_key, jdoc_node_str_bounds
  public :: jdoc_array_len, jdoc_array_item, jdoc_object_len, jdoc_object_entry

  integer(i32), parameter :: CDD_MAX_JNODES = 512_i32  !< Max JSON DOM value nodes per document.
  integer(i32), parameter :: CDD_MAX_JPROPS = 512_i32  !< Max object key/value pairs per document.
  integer(i32), parameter :: CDD_MAX_JITEMS = 1024_i32 !< Max array item links per document.

  integer(i32), parameter :: JVAL_NULL = 0_i32         !< JSON 'null' value.
  integer(i32), parameter :: JVAL_BOOL = 1_i32         !< JSON boolean value.
  integer(i32), parameter :: JVAL_INT = 2_i32          !< JSON 64-bit integer value.
  integer(i32), parameter :: JVAL_FLOAT = 3_i32        !< JSON 64-bit floating-point value.
  integer(i32), parameter :: JVAL_STRING = 4_i32       !< JSON decoded string value.
  integer(i32), parameter :: JVAL_ARRAY = 5_i32        !< JSON ordered array value.
  integer(i32), parameter :: JVAL_OBJECT = 6_i32       !< JSON insertion-ordered object value.

  !> @brief Single value node stored in cdd_json_doc_t%nodes.
  type :: cdd_json_node_t
    integer(i32) :: kind = JVAL_NULL      !< Value category (JVAL_*).
    logical(lk) :: bool_val = .false._lk  !< Stored boolean when kind == JVAL_BOOL.
    integer(i64) :: int_val = 0_i64       !< Stored 64-bit integer when kind == JVAL_INT.
    real(r64) :: float_val = 0.0_r64      !< Stored 64-bit float when kind == JVAL_FLOAT.
    integer(i32) :: str_off = 1_i32       !< 1-based start offset in doc%str_pool%raw.
    integer(i32) :: str_len = 0_i32       !< Byte length in doc%str_pool%raw (string or float lexeme).
    integer(i32) :: head = 0_i32          !< First prop/item index for JVAL_OBJECT / JVAL_ARRAY.
    integer(i32) :: tail = 0_i32          !< Last prop/item index for O(1) ordered append.
    integer(i32) :: count = 0_i32         !< Child count for JVAL_OBJECT / JVAL_ARRAY.
  end type cdd_json_node_t

  !> @brief Single insertion-ordered object member entry in cdd_json_doc_t%props.
  type :: cdd_json_prop_t
    integer(i32) :: key_off = 1_i32       !< 1-based start offset of key in doc%str_pool%raw.
    integer(i32) :: key_len = 0_i32       !< Byte length of key in doc%str_pool%raw.
    integer(i32) :: val_node = 0_i32      !< 1-based value node index in doc%nodes.
    integer(i32) :: next_prop = 0_i32     !< Next property index in object (0 if tail).
  end type cdd_json_prop_t

  !> @brief Single ordered element link in cdd_json_doc_t%items.
  type :: cdd_json_item_t
    integer(i32) :: val_node = 0_i32      !< 1-based value node index in doc%nodes.
    integer(i32) :: next_item = 0_i32     !< Next item index in array (0 if tail).
  end type cdd_json_item_t

  !> @brief Complete self-contained JSON document arena with string pool.
  type :: cdd_json_doc_t
    integer(i32) :: root = 0_i32                         !< Root node index (0 if empty).
    integer(i32) :: node_count = 0_i32                   !< Allocated nodes in nodes(1:node_count).
    integer(i32) :: prop_count = 0_i32                   !< Allocated object properties.
    integer(i32) :: item_count = 0_i32                   !< Allocated array item links.
    integer(i32) :: max_nodes = CDD_MAX_JNODES           !< Effective node capacity limit.
    integer(i32) :: max_props = CDD_MAX_JPROPS           !< Effective property capacity limit.
    integer(i32) :: max_items = CDD_MAX_JITEMS           !< Effective array-item capacity limit.
    type(cdd_buffer_t) :: str_pool                       !< Shared string & float-lexeme byte pool.
    type(cdd_json_node_t) :: nodes(CDD_MAX_JNODES)       !< Value node arena.
    type(cdd_json_prop_t) :: props(CDD_MAX_JPROPS)       !< Object property arena.
    type(cdd_json_item_t) :: items(CDD_MAX_JITEMS)       !< Array element arena.
  end type cdd_json_doc_t

contains

  !> @brief Initialize an empty JSON DOM arena with full default capacities.
  !> @param[out] doc JSON document arena to initialize.
  pure subroutine jdoc_init(doc)
    type(cdd_json_doc_t), intent(out) :: doc

    doc%root = 0_i32
    doc%node_count = 0_i32
    doc%prop_count = 0_i32
    doc%item_count = 0_i32
    doc%max_nodes = CDD_MAX_JNODES
    doc%max_props = CDD_MAX_JPROPS
    doc%max_items = CDD_MAX_JITEMS
    call buf_init(doc%str_pool)
  end subroutine jdoc_init

  !> @brief Initialize an empty JSON DOM arena with custom bounded quotas for testing.
  !> @param[out] doc   JSON document arena.
  !> @param[in]  mn    Max nodes (1..CDD_MAX_JNODES).
  !> @param[in]  mp    Max object properties (1..CDD_MAX_JPROPS).
  !> @param[in]  mi    Max array items (1..CDD_MAX_JITEMS).
  !> @param[out] err   Error status if bounds are invalid.
  pure subroutine jdoc_init_bounded(doc, mn, mp, mi, err)
    type(cdd_json_doc_t), intent(out) :: doc
    integer(i32), intent(in) :: mn
    integer(i32), intent(in) :: mp
    integer(i32), intent(in) :: mi
    type(cdd_error_t), intent(out) :: err

    call jdoc_init(doc)
    call cdd_error_clear(err)
    if (mn < 1_i32 .or. mn > CDD_MAX_JNODES .or. &
        mp < 1_i32 .or. mp > CDD_MAX_JPROPS .or. &
        mi < 1_i32 .or. mi > CDD_MAX_JITEMS) then
      call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, "Invalid JSON DOM capacity bounds")
      return
    end if
    doc%max_nodes = mn
    doc%max_props = mp
    doc%max_items = mi
  end subroutine jdoc_init_bounded

  !> @brief Allocate a null value node in doc.
  !> @param[inout] doc      Target JSON document.
  !> @param[out]   node_idx Allocated 1-based node index.
  !> @param[out]   err      Error status if arena is full.
  pure subroutine jdoc_add_null(doc, node_idx, err)
    type(cdd_json_doc_t), intent(inout) :: doc
    integer(i32), intent(out) :: node_idx
    type(cdd_error_t), intent(out) :: err

    call alloc_node(doc, JVAL_NULL, node_idx, err)
  end subroutine jdoc_add_null

  !> @brief Allocate a boolean value node in doc.
  !> @param[inout] doc      Target JSON document.
  !> @param[in]    val      Boolean value.
  !> @param[out]   node_idx Allocated 1-based node index.
  !> @param[out]   err      Error status if arena is full.
  pure subroutine jdoc_add_bool(doc, val, node_idx, err)
    type(cdd_json_doc_t), intent(inout) :: doc
    logical(lk), intent(in) :: val
    integer(i32), intent(out) :: node_idx
    type(cdd_error_t), intent(out) :: err

    call alloc_node(doc, JVAL_BOOL, node_idx, err)
    if (err%code == 0_i32) then
      doc%nodes(node_idx)%bool_val = val
    end if
  end subroutine jdoc_add_bool

  !> @brief Allocate a 64-bit integer value node in doc.
  !> @param[inout] doc      Target JSON document.
  !> @param[in]    val      64-bit integer value.
  !> @param[out]   node_idx Allocated 1-based node index.
  !> @param[out]   err      Error status if arena is full.
  pure subroutine jdoc_add_int(doc, val, node_idx, err)
    type(cdd_json_doc_t), intent(inout) :: doc
    integer(i64), intent(in) :: val
    integer(i32), intent(out) :: node_idx
    type(cdd_error_t), intent(out) :: err

    call alloc_node(doc, JVAL_INT, node_idx, err)
    if (err%code == 0_i32) then
      doc%nodes(node_idx)%int_val = val
    end if
  end subroutine jdoc_add_int

  !> @brief Allocate a floating-point value node in doc, optionally caching its raw lexeme.
  !> @param[inout] doc      Target JSON document.
  !> @param[in]    val      64-bit floating-point value.
  !> @param[in]    lexeme   Optional raw source lexeme (empty string to format from val).
  !> @param[out]   node_idx Allocated 1-based node index.
  !> @param[out]   err      Error status if arena or string pool is full.
  pure subroutine jdoc_add_float(doc, val, lexeme, node_idx, err)
    type(cdd_json_doc_t), intent(inout) :: doc
    real(r64), intent(in) :: val
    character(len=*, kind=c_k), intent(in) :: lexeme
    integer(i32), intent(out) :: node_idx
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: off, slen

    call alloc_node(doc, JVAL_FLOAT, node_idx, err)
    if (err%code /= 0_i32) return
    doc%nodes(node_idx)%float_val = val
    slen = len(lexeme, kind=i32)
    if (slen > 0_i32) then
      off = doc%str_pool%len + 1_i32
      call buf_append_str(doc%str_pool, lexeme, err)
      if (err%code /= 0_i32) return
      doc%nodes(node_idx)%str_off = off
      doc%nodes(node_idx)%str_len = slen
    end if
  end subroutine jdoc_add_float

  !> @brief Allocate a decoded string value node in doc.
  !> @param[inout] doc      Target JSON document.
  !> @param[in]    val      Decoded UTF-8/ASCII string content.
  !> @param[out]   node_idx Allocated 1-based node index.
  !> @param[out]   err      Error status if arena or string pool is full.
  pure subroutine jdoc_add_string(doc, val, node_idx, err)
    type(cdd_json_doc_t), intent(inout) :: doc
    character(len=*, kind=c_k), intent(in) :: val
    integer(i32), intent(out) :: node_idx
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: off, slen

    call alloc_node(doc, JVAL_STRING, node_idx, err)
    if (err%code /= 0_i32) return
    off = doc%str_pool%len + 1_i32
    slen = len(val, kind=i32)
    call buf_append_str(doc%str_pool, val, err)
    if (err%code /= 0_i32) return
    doc%nodes(node_idx)%str_off = off
    doc%nodes(node_idx)%str_len = slen
  end subroutine jdoc_add_string

  !> @brief Allocate an empty JSON array node in doc.
  !> @param[inout] doc      Target JSON document.
  !> @param[out]   node_idx Allocated 1-based node index.
  !> @param[out]   err      Error status if arena is full.
  pure subroutine jdoc_add_array(doc, node_idx, err)
    type(cdd_json_doc_t), intent(inout) :: doc
    integer(i32), intent(out) :: node_idx
    type(cdd_error_t), intent(out) :: err

    call alloc_node(doc, JVAL_ARRAY, node_idx, err)
  end subroutine jdoc_add_array

  !> @brief Allocate an empty JSON object node in doc.
  !> @param[inout] doc      Target JSON document.
  !> @param[out]   node_idx Allocated 1-based node index.
  !> @param[out]   err      Error status if arena is full.
  pure subroutine jdoc_add_object(doc, node_idx, err)
    type(cdd_json_doc_t), intent(inout) :: doc
    integer(i32), intent(out) :: node_idx
    type(cdd_error_t), intent(out) :: err

    call alloc_node(doc, JVAL_OBJECT, node_idx, err)
  end subroutine jdoc_add_object

  !> @brief Append a child value node to a JVAL_ARRAY node in O(1) time.
  !> @param[inout] doc      Target JSON document.
  !> @param[in]    arr_node Array node index.
  !> @param[in]    val_node Element value node index.
  !> @param[out]   err      Error status if arr_node is invalid or item arena is full.
  pure subroutine jdoc_array_append(doc, arr_node, val_node, err)
    type(cdd_json_doc_t), intent(inout) :: doc
    integer(i32), intent(in) :: arr_node
    integer(i32), intent(in) :: val_node
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: item_idx, prev_tail

    call cdd_error_clear(err)
    if (arr_node < 1_i32 .or. arr_node > doc%node_count) then
      call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, "Invalid array node index")
      return
    end if
    if (doc%nodes(arr_node)%kind /= JVAL_ARRAY) then
      call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, "Target node is not JVAL_ARRAY")
      return
    end if
    if (doc%item_count >= doc%max_items) then
      call cdd_error_set_simple(err, CDD_ERR_BUFFER_OVERFLOW, "JSON array item arena full")
      return
    end if

    doc%item_count = doc%item_count + 1_i32
    item_idx = doc%item_count
    doc%items(item_idx)%val_node = val_node
    doc%items(item_idx)%next_item = 0_i32

    if (doc%nodes(arr_node)%head == 0_i32) then
      doc%nodes(arr_node)%head = item_idx
    else
      prev_tail = doc%nodes(arr_node)%tail
      doc%items(prev_tail)%next_item = item_idx
    end if
    doc%nodes(arr_node)%tail = item_idx
    doc%nodes(arr_node)%count = doc%nodes(arr_node)%count + 1_i32
  end subroutine jdoc_array_append

  !> @brief Insert or update a key/value property on a JVAL_OBJECT node preserving key order.
  !> @param[inout] doc        Target JSON document.
  !> @param[in]    obj_node   Object node index.
  !> @param[in]    key        Decoded property key string.
  !> @param[in]    val_node   Child value node index.
  !> @param[in]    reject_dup When true, return CDD_ERR_JSON_PARSE if key already exists.
  !> @param[out]   err        Error status record.
  pure subroutine jdoc_object_set(doc, obj_node, key, val_node, reject_dup, err)
    type(cdd_json_doc_t), intent(inout) :: doc
    integer(i32), intent(in) :: obj_node
    character(len=*, kind=c_k), intent(in) :: key
    integer(i32), intent(in) :: val_node
    logical(lk), intent(in) :: reject_dup
    type(cdd_error_t), intent(out) :: err
    integer(i32) :: cur, prop_idx, prev_tail, koff, klen

    call cdd_error_clear(err)
    if (obj_node < 1_i32 .or. obj_node > doc%node_count) then
      call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, "Invalid object node index")
      return
    end if
    if (doc%nodes(obj_node)%kind /= JVAL_OBJECT) then
      call cdd_error_set_simple(err, CDD_ERR_INVALID_OPTION, "Target node is not JVAL_OBJECT")
      return
    end if

    cur = doc%nodes(obj_node)%head
    do while (cur /= 0_i32)
      koff = doc%props(cur)%key_off
      klen = doc%props(cur)%key_len
      if (str_eq(doc%str_pool%raw(koff:koff + klen - 1_i32), key)) then
        if (reject_dup) then
          call cdd_error_set_simple(err, CDD_ERR_JSON_PARSE, "Duplicate key in JSON object")
          return
        end if
        doc%props(cur)%val_node = val_node
        return
      end if
      cur = doc%props(cur)%next_prop
    end do

    if (doc%prop_count >= doc%max_props) then
      call cdd_error_set_simple(err, CDD_ERR_BUFFER_OVERFLOW, "JSON object property arena full")
      return
    end if

    koff = doc%str_pool%len + 1_i32
    klen = len(key, kind=i32)
    call buf_append_str(doc%str_pool, key, err)
    if (err%code /= 0_i32) return

    doc%prop_count = doc%prop_count + 1_i32
    prop_idx = doc%prop_count
    doc%props(prop_idx)%key_off = koff
    doc%props(prop_idx)%key_len = klen
    doc%props(prop_idx)%val_node = val_node
    doc%props(prop_idx)%next_prop = 0_i32

    if (doc%nodes(obj_node)%head == 0_i32) then
      doc%nodes(obj_node)%head = prop_idx
    else
      prev_tail = doc%nodes(obj_node)%tail
      doc%props(prev_tail)%next_prop = prop_idx
    end if
    doc%nodes(obj_node)%tail = prop_idx
    doc%nodes(obj_node)%count = doc%nodes(obj_node)%count + 1_i32
  end subroutine jdoc_object_set

  !> @brief Look up a property value node on an object by exact key name (0 if absent).
  !> @param[in] doc      JSON document arena.
  !> @param[in] obj_node Object node index.
  !> @param[in] key      Property key to find.
  !> @return Child value node index, or 0 if not found / not an object.
  pure function jdoc_get_key(doc, obj_node, key) result(val_node)
    type(cdd_json_doc_t), intent(in) :: doc
    integer(i32), intent(in) :: obj_node
    character(len=*, kind=c_k), intent(in) :: key
    integer(i32) :: val_node
    integer(i32) :: cur, koff, klen

    val_node = 0_i32
    if (obj_node < 1_i32 .or. obj_node > doc%node_count) return
    if (doc%nodes(obj_node)%kind /= JVAL_OBJECT) return

    cur = doc%nodes(obj_node)%head
    do while (cur /= 0_i32)
      koff = doc%props(cur)%key_off
      klen = doc%props(cur)%key_len
      if (str_eq(doc%str_pool%raw(koff:koff + klen - 1_i32), key)) then
        val_node = doc%props(cur)%val_node
        return
      end if
      cur = doc%props(cur)%next_prop
    end do
  end function jdoc_get_key

  !> @brief Check whether a JVAL_OBJECT node contains a property with the given key.
  !> @param[in] doc      JSON document arena.
  !> @param[in] obj_node Object node index.
  !> @param[in] key      Property key to check.
  !> @return True if key exists on obj_node.
  pure function jdoc_has_key(doc, obj_node, key) result(present_flag)
    type(cdd_json_doc_t), intent(in) :: doc
    integer(i32), intent(in) :: obj_node
    character(len=*, kind=c_k), intent(in) :: key
    logical(lk) :: present_flag

    present_flag = (jdoc_get_key(doc, obj_node, key) > 0_i32)
  end function jdoc_has_key

  !> @brief Retrieve 1-based slice bounds [lo..hi] inside doc%str_pool%raw for a node.
  !> @param[in]  doc      JSON document arena.
  !> @param[in]  node_idx Node index.
  !> @param[out] lo       Start index in doc%str_pool%raw.
  !> @param[out] hi       End index in doc%str_pool%raw (lo > hi when empty).
  pure subroutine jdoc_node_str_bounds(doc, node_idx, lo, hi)
    type(cdd_json_doc_t), intent(in) :: doc
    integer(i32), intent(in) :: node_idx
    integer(i32), intent(out) :: lo
    integer(i32), intent(out) :: hi

    if (node_idx < 1_i32 .or. node_idx > doc%node_count) then
      lo = 1_i32
      hi = 0_i32
      return
    end if
    lo = doc%nodes(node_idx)%str_off
    hi = lo + doc%nodes(node_idx)%str_len - 1_i32
  end subroutine jdoc_node_str_bounds

  !> @brief Return the element count of a JVAL_ARRAY node (0 if invalid/non-array).
  !> @param[in] doc      JSON document arena.
  !> @param[in] arr_node Array node index.
  !> @return Number of elements in array.
  pure function jdoc_array_len(doc, arr_node) result(n)
    type(cdd_json_doc_t), intent(in) :: doc
    integer(i32), intent(in) :: arr_node
    integer(i32) :: n

    n = 0_i32
    if (arr_node < 1_i32 .or. arr_node > doc%node_count) return
    if (doc%nodes(arr_node)%kind /= JVAL_ARRAY) return
    n = doc%nodes(arr_node)%count
  end function jdoc_array_len

  !> @brief Return the 1-based idx-th element node of a JVAL_ARRAY (0 if out of range).
  !> @param[in] doc      JSON document arena.
  !> @param[in] arr_node Array node index.
  !> @param[in] idx      1-based element position.
  !> @return Child value node index, or 0 if out of range.
  pure function jdoc_array_item(doc, arr_node, idx) result(val_node)
    type(cdd_json_doc_t), intent(in) :: doc
    integer(i32), intent(in) :: arr_node
    integer(i32), intent(in) :: idx
    integer(i32) :: val_node
    integer(i32) :: cur, pos

    val_node = 0_i32
    if (idx < 1_i32 .or. idx > jdoc_array_len(doc, arr_node)) return
    cur = doc%nodes(arr_node)%head
    pos = 1_i32
    do while (pos < idx)
      cur = doc%items(cur)%next_item
      pos = pos + 1_i32
    end do
    val_node = doc%items(cur)%val_node
  end function jdoc_array_item

  !> @brief Return the property count of a JVAL_OBJECT node (0 if invalid/non-object).
  !> @param[in] doc      JSON document arena.
  !> @param[in] obj_node Object node index.
  !> @return Number of properties on object.
  pure function jdoc_object_len(doc, obj_node) result(n)
    type(cdd_json_doc_t), intent(in) :: doc
    integer(i32), intent(in) :: obj_node
    integer(i32) :: n

    n = 0_i32
    if (obj_node < 1_i32 .or. obj_node > doc%node_count) return
    if (doc%nodes(obj_node)%kind /= JVAL_OBJECT) return
    n = doc%nodes(obj_node)%count
  end function jdoc_object_len

  !> @brief Retrieve the 1-based idx-th property's key bounds and value node from JVAL_OBJECT.
  !> @param[in]  doc      JSON document arena.
  !> @param[in]  obj_node Object node index.
  !> @param[in]  idx      1-based property position.
  !> @param[out] key_lo   Start offset of key in doc%str_pool%raw.
  !> @param[out] key_hi   End offset of key in doc%str_pool%raw.
  !> @param[out] val_node Child value node index (0 if out of range).
  pure subroutine jdoc_object_entry(doc, obj_node, idx, key_lo, key_hi, val_node)
    type(cdd_json_doc_t), intent(in) :: doc
    integer(i32), intent(in) :: obj_node
    integer(i32), intent(in) :: idx
    integer(i32), intent(out) :: key_lo
    integer(i32), intent(out) :: key_hi
    integer(i32), intent(out) :: val_node
    integer(i32) :: cur, pos

    key_lo = 1_i32
    key_hi = 0_i32
    val_node = 0_i32
    if (idx < 1_i32 .or. idx > jdoc_object_len(doc, obj_node)) return
    cur = doc%nodes(obj_node)%head
    pos = 1_i32
    do while (pos < idx)
      cur = doc%props(cur)%next_prop
      pos = pos + 1_i32
    end do
    key_lo = doc%props(cur)%key_off
    key_hi = key_lo + doc%props(cur)%key_len - 1_i32
    val_node = doc%props(cur)%val_node
  end subroutine jdoc_object_entry

  !> @brief Internal node slot allocator initializing scalar/list defaults.
  !> @param[inout] doc      Target JSON document.
  !> @param[in]    kind     Value kind (JVAL_*).
  !> @param[out]   node_idx Allocated 1-based node index.
  !> @param[out]   err      Error status if node arena is full.
  pure subroutine alloc_node(doc, kind, node_idx, err)
    type(cdd_json_doc_t), intent(inout) :: doc
    integer(i32), intent(in) :: kind
    integer(i32), intent(out) :: node_idx
    type(cdd_error_t), intent(out) :: err

    call cdd_error_clear(err)
    if (doc%node_count >= doc%max_nodes) then
      node_idx = 0_i32
      call cdd_error_set_simple(err, CDD_ERR_BUFFER_OVERFLOW, "JSON node arena full")
      return
    end if
    doc%node_count = doc%node_count + 1_i32
    node_idx = doc%node_count
    doc%nodes(node_idx)%kind = kind
    doc%nodes(node_idx)%bool_val = .false._lk
    doc%nodes(node_idx)%int_val = 0_i64
    doc%nodes(node_idx)%float_val = 0.0_r64
    doc%nodes(node_idx)%str_off = 1_i32
    doc%nodes(node_idx)%str_len = 0_i32
    doc%nodes(node_idx)%head = 0_i32
    doc%nodes(node_idx)%tail = 0_i32
    doc%nodes(node_idx)%count = 0_i32
  end subroutine alloc_node

end module cdd_json_dom
