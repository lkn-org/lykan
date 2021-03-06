defmodule Lykan.Config do
  @derive [Poison.Encoder]
  defstruct [
    :maps,
    :default_map,
  ]

  def from_env!(default) do
    file = case System.get_env("LYKAN_CONFIG_FILE") do
             nil -> default
             path -> path
           end

    from_file!(file)
  end

  def from_file!(file) do
    {:ok, str} = File.read(file)
    Poison.decode!(str, as: %__MODULE__{})
  end
end
