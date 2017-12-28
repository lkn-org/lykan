use Lkn.Prelude
import Lkn.Core.Puppet, only: [defpuppet: 2]

defpuppet Lykan.Character do
  defmodule Appearance do
    use Lykan.System.Appearance.Component

    def set_color(key, c, no_state) do
      write(key, :color, c)
      no_state
    end

    def get_color(key, no_state) do
      Option.some(c) = read(key, :color)
      {c, no_state}
    end

    def init_state(key) do
      {:ok, :no_state}
    end
  end

  @components [Appearance]

  def start_link(key) do
    Lkn.Core.Entity.start_link(__MODULE__, key, :no_args)
  end

  def init_properties(_no_args) do
    %{
      :color => "black"
    }
  end
end
