defmodule Lykan.Config do
  @derive [Poison.Encoder]
  defstruct [
    :maps,
    :default_map,
  ]

  def from_file!(file) do
    {:ok, str} = File.read(file)
    Poison.decode!(str, as: %__MODULE__{})
  end
end
