defmodule BPF.IR do
  @moduledoc false

  defmodule Segment do
    @moduledoc false

    defstruct [
      :type,
      :name,
      :value,
      :size,
      :offset,
      unit: 1,
      endianness: :big,
      signedness: :unsigned
    ]

    @type segment_type :: :literal | :binding | :skip

    @type t :: %__MODULE__{
            type: segment_type(),
            name: atom() | nil,
            value: integer() | nil,
            size: non_neg_integer() | :rest,
            offset: non_neg_integer(),
            unit: pos_integer(),
            endianness: :big | :little | :native,
            signedness: :signed | :unsigned
          }

    @doc "Create a literal segment (fixed value match)"
    def literal(value, size, offset, opts \\ []) do
      %__MODULE__{
        type: :literal,
        name: nil,
        value: value,
        size: size,
        offset: offset,
        unit: Keyword.get(opts, :unit, 1),
        endianness: Keyword.get(opts, :endianness, :big),
        signedness: Keyword.get(opts, :signedness, :unsigned)
      }
    end

    @doc "Create a binding segment (captured variable)"
    def binding(name, size, offset, opts \\ []) do
      %__MODULE__{
        type: :binding,
        name: name,
        value: nil,
        size: size,
        offset: offset,
        unit: Keyword.get(opts, :unit, 1),
        endianness: Keyword.get(opts, :endianness, :big),
        signedness: Keyword.get(opts, :signedness, :unsigned)
      }
    end

    @doc "Create a skip segment (ignored bytes)"
    def skip(size, offset, opts \\ []) do
      %__MODULE__{
        type: :skip,
        name: nil,
        value: nil,
        size: size,
        offset: offset,
        unit: Keyword.get(opts, :unit, 1),
        endianness: Keyword.get(opts, :endianness, :big),
        signedness: Keyword.get(opts, :signedness, :unsigned)
      }
    end

    @doc "Get the size in bits for this segment"
    def bits(%__MODULE__{size: :rest}), do: :rest
    def bits(%__MODULE__{size: size, unit: unit}), do: size * unit
  end

  defmodule Pattern do
    @moduledoc false

    defstruct [:segments]

    @type t :: %__MODULE__{
            segments: [Segment.t()]
          }

    def new(segments) do
      %__MODULE__{segments: segments}
    end

    @doc "Get all bindings from the pattern as a map of name => segment"
    def bindings(%__MODULE__{segments: segments}) do
      segments
      |> Enum.filter(&(&1.type == :binding))
      |> Map.new(&{&1.name, &1})
    end

    @doc "Get all literal segments that need to be checked"
    def literals(%__MODULE__{segments: segments}) do
      Enum.filter(segments, &(&1.type == :literal))
    end
  end

  defmodule Guard do
    @moduledoc false

    @type comparison :: :eq | :neq | :lt | :gt | :lte | :gte
    @type logical :: :and | :or | :not
    @type bitwise :: :band | :bor | :bxor | :bnot
    @type arithmetic :: :add | :sub | :mul | :div

    @type operand ::
            {:binding, atom()}
            | {:literal, integer()}
            | t()

    @type t ::
            {:compare, comparison(), operand(), operand()}
            | {:logical, logical(), t(), t() | nil}
            | {:bitwise, bitwise(), operand(), operand() | nil}
            | {:arith, arithmetic(), operand(), operand()}

    @doc "Create a comparison guard"
    def compare(op, left, right), do: {:compare, op, left, right}

    @doc "Create a logical AND guard"
    def logical_and(left, right), do: {:logical, :and, left, right}

    @doc "Create a logical OR guard"
    def logical_or(left, right), do: {:logical, :or, left, right}

    @doc "Create a logical NOT guard"
    def logical_not(expr), do: {:logical, :not, expr, nil}

    @doc "Create a bitwise operation"
    def bitwise(op, left, right), do: {:bitwise, op, left, right}

    @doc "Create an arithmetic operation"
    def arith(op, left, right), do: {:arith, op, left, right}

    @doc "Reference a bound variable"
    def binding(name), do: {:binding, name}

    @doc "A literal value"
    def literal(value), do: {:literal, value}
  end

  defmodule Clause do
    @moduledoc false

    defstruct [:pattern, :guard, :action]

    @type action :: :accept | :reject | {:accept, non_neg_integer()}

    @type t :: %__MODULE__{
            pattern: Pattern.t(),
            guard: Guard.t() | nil,
            action: action()
          }

    def new(pattern, guard \\ nil, action \\ :accept) do
      %__MODULE__{
        pattern: pattern,
        guard: guard,
        action: action
      }
    end
  end
end
