defmodule Lykan.Commands do
  defmodule SetDirection do
    defstruct [
      :angle
    ]
  end

  defmodule LookAt do
    defstruct [
      :angle
    ]
  end

  defmodule StartsWalking do
    defstruct []
  end
end

defmodule Lykan.Command do
  @derive [Poison.Encoder]
  defstruct [
    :opcode,
    :arguments,
  ]

  @commands %{
    "SET_DIRECTION" => %Lykan.Commands.SetDirection{},
    "LOOK_AT" => %Lykan.Commands.LookAt{},
    "STARTS_WALKING" => %Lykan.Commands.StartsWalking{},
  }

  def decode(json) do
    cmd = Poison.decode(json, as: %__MODULE__{})

    case cmd do
      {:ok, cmd} ->
        mod = Enum.reduce_while(@commands, nil, fn ({k, v}, acc) ->
            if k == cmd.opcode do
              {:halt, v}
            else
              {:cont, acc}
            end
          end)

        if mod do
          # TODO: Find a better way to do it
          case Poison.decode(Poison.encode!(cmd.arguments), as: mod) do
            {:ok, cmd} -> cmd
            _ -> json
          end
        end
      _ -> json
    end
  end
end
