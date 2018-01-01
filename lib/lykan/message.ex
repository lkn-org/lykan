use Lkn.Prelude

defprotocol Lykan.Message.Protocol do
  def opcode(self)
end

defmodule Lykan.Message do
  @derive [Poison.Encoder]
  defstruct [
    :opcode,
    :message,
  ]

  def opcode(msg) do
    Lykan.Message.Protocol.opcode(msg)
  end

  def encode(msg) do
    Poison.encode(%__MODULE__{
          opcode: opcode(msg),
          message: msg,
        })
  end

  def encode!(msg) do
    {:ok, json} = encode(msg)
    json
  end

  defmacro defmessage(name, do: block) do
    block = case block do
              {:__block__, _, x} -> x
              x -> [x]
            end

    {opcode, content, legit} = parse_block(block, Option.nothing(), Option.nothing(), [])

    quote do
      defmodule unquote(name) do
        @derive [Poison.Encoder]
        defstruct unquote(content)

        unquote(legit)
      end
      defimpl Lykan.Message.Protocol, for: unquote(name) do
        def opcode(_) do
          unquote(opcode)
        end
      end
    end
  end

  defp parse_block(block, opcode, content, legit) do
    case block do
      [{:opcode, _, [op]}|rest] ->
        case opcode do
          Option.nothing() -> parse_block(rest, Option.some(op), content, legit)
          _ -> raise "Trying to redefine opcode"
        end
      [{:content, _, [cont]}|rest] ->
        case content do
          Option.nothing() -> parse_block(rest, opcode, Option.some(cont), legit)
          _ -> raise "Trying to redefine content"
        end
      [x|rest] ->
        parse_block(rest, opcode, content, [x|legit])
      [] ->
        case {opcode, content} do
          {Option.some(op), Option.some(cont)} ->
            {op, cont, Enum.reverse(legit)}
          _ ->
            raise "Missing `content` or `field`"
        end
    end
  end
end
