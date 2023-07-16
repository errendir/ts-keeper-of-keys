export type DEEP_ACCESS_TYPE<T> = Simplify<DEEP_ACCESS_TYPE_<{ "": T }, {}>>;

type IsEmptyObject<T> = [keyof T] extends [never] ? true : false;

type DEEP_ACCESS_TYPE_<PROCESSING, FINISHED> =
  IsEmptyObject<PROCESSING> extends true
    ? FINISHED
    : // In each unwinding round the `DEEP_ACCESS_TYPE_` processes the keys in the `PROCESSING` object
    // Some of the new keys will finally reach types we don't want to recurse into,
    // those will be put in the `FINISHED` object, the rest will be put inth the `PROCESSING` to keep unwinding them
    MAP_TO_UNWIND_LEVEL<PROCESSING> extends infer NEXT_LEVEL_OBJECTS
    ? DEEP_ACCESS_TYPE_<
        FOLD_SUBOBJECTS<
          ALL_OBJECTS_WITH_KEY<NEXT_LEVEL_OBJECTS, "CONTINUE_ITERATING">
        >,
        FINISHED &
          FOLD_SUBOBJECTS<
            ALL_OBJECTS_WITH_KEY<NEXT_LEVEL_OBJECTS, "FINISH_RECURSION">
          >
      >
    : never;

type MAP_TO_UNWIND_LEVEL<T> = {
  [K in keyof T & string]: UNWIND_LEVEL<K, T[K]>;
}[keyof T & string];

type ALL_OBJECTS_WITH_KEY<T, K extends string> = T extends {
  [KK in K]: infer Z;
}
  ? Z
  : never;

type UNWIND_LEVEL<ParentKey extends string, T> = IsPrimitiveType<T> extends true
  ? {
      // Mark the object with the "FINISH_RECURSION" key to make sure `DEEP_ACCESS_TYPE_` ignores it in all subsequent unwinding rounds
      FINISH_RECURSION: { [K in ParentKey]: T };
    }
  : T extends any[]
  ? UNWIND_LEVEL<ParentKey, TUPLE_TO_OBJECT_SAFE<T>>
  : T extends readonly any[]
  ? UNWIND_LEVEL<ParentKey, TUPLE_TO_OBJECT_SAFE<T>>
  : {
      // Mark the object with the "CONTINUE_ITERATING" key to make sure `DEEP_ACCESS_TYPE_` keeps unwinding it
      CONTINUE_ITERATING: {
        [Key in keyof T & (string | number) as `${ParentKey}/${Key}`]: T[Key];
      };
    };

// TESTS:

const example1 = {
  welcome: "everyone",
  num: 5,
  object: {
    array_test: [{ age: 2 }, { name: "Jeremy" }],
    tuple_test: [5, 4, 3, 2, 1] as const,
    simple_prop: "bar",
    nest: {
      further: {
        further: {
          further: {
            further: {
              further: {
                further: {
                  further: {
                    further: {
                      further: {
                        further: {
                          further: {
                            further: {
                              further: {
                                further: {
                                  further: {
                                    further: 111,
                                  },
                                },
                              },
                            },
                          },
                        },
                      },
                    },
                  },
                },
              },
            },
          },
        },
      },
    },
  },
};

type TEST_1 = DEEP_ACCESS_TYPE<
  typeof example1
>["/object/nest/further/further/further/further/further/further/further/further/further/further/further/further/further/further/further/further"];

type TEST_2 = DEEP_ACCESS_TYPE<readonly [1, 2, "hello"]>;
type TEST_3 = DEEP_ACCESS_TYPE<[1, 2, "hello"]>;

type TEST_4 = DEEP_ACCESS_TYPE<{ p: number; a: string[] }>;
type TEST_4_ACCESS_1 = TEST_4["/a/13142"];
type TEST_4_ACCESS_2 = TEST_4["/p"];

// HELPER TYPES (PERHAPS COULD BE SIMPLIFIED):
type Simplify<T> = { [K in keyof T]: T[K] } & {};

type ALL_KEYS<Z> = Z extends object ? keyof Z : never;
type GET_VAL<T, K extends string> = T extends { [Z in K]: infer K }
  ? unknown extends K
    ? never
    : K
  : never;
// Turns a union of object types into an intersection
type FOLD_SUBOBJECTS<T> = {
  [K in ALL_KEYS<T>]: K extends string ? GET_VAL<T, K> : never;
};
type M = FOLD_SUBOBJECTS<{ a: number } | { b: string }>;

type IsUnknownOrAny<T> = unknown extends T ? true : false;
type IsAny<T> = 0 extends 1 & T ? true : false;
type IsPrimitiveType<T> = T extends
  | number
  | string
  | boolean
  | symbol
  | null
  | undefined
  ? true
  : IsUnknownOrAny<T>;

type TUPLE_TO_PAIRS<T extends readonly any[]> = { [K in keyof T]: [K, T[K]] };
type TUPLE_TO_OBJECT_SAFE<Tuple> = Tuple extends readonly any[]
  ? TUPLE_TO_PAIRS<Tuple> extends readonly any[]
    ? TUPLE_TO_OBJECT<TUPLE_TO_PAIRS<Tuple>>
    : never
  : never;
type TUPLE_TO_OBJECT<
  Pairs extends readonly any[],
  Acc = {}
> = Pairs["length"] extends 0
  ? Acc
  : Pairs extends readonly [
      infer Head extends [string | number, any],
      ...infer Tail extends readonly [string | number, any][]
    ]
  ? TUPLE_TO_OBJECT<Tail, Acc & { [T in Head[0]]: Head[1] }>
  : Pairs extends readonly [number, infer Type][]
  ? { [T in `${number}`]: Type }
  : never;

// Convert a tuple to a simple object mapping only the indices to their corresponding types
type TUPLE_TEST_1 = TUPLE_TO_OBJECT<
  TUPLE_TO_PAIRS<[number, string, { a: boolean }]>
>;
type TUPLE_TEST_2 = TUPLE_TO_OBJECT<TUPLE_TO_PAIRS<number[]>>;
