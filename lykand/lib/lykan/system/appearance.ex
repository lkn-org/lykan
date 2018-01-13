use Lkn.Prelude
import Lkn.Core.System, only: [defsystem: 2]
import Lkn.Core.Component, only: [defcomponent: 2]
import Lykan.Message, only: [defmessage: 2]

defsystem Lykan.System.Appearance do
  defcomponent Component do
    @system Lykan.System.Appearance

    @call get_color() :: String.t
    @cast set_color(c :: String.t)
  end

  @map Component
  @puppet Component

  defmessage ColorChange do
    opcode "COLOR_CHANGE"

    content [
      :puppet_key,
      :color,
    ]
  end

  def init_state(_instance_key, _map_key) do
    :ok
  end

  def puppet_enter(_no_state, _instance_key, map_key, _puppets, key, _opts) do
    c = Component.get_color(map_key)
    Component.set_color(key, c)

    notify(&Lykan.Puppeteer.notify(&1, ColorChange.craft(key, c)))

    cast_return()
  end

  def puppet_leave(_no_state, _instance_key, _map_key, _puppets, _key) do
    cast_return()
  end

  cast change_puppet_color(key :: Lkn.Core.Puppet.k, c :: String.t) do
    Component.set_color(key, c)

    notify(&Lykan.Puppeteer.notify(&1, ColorChange.craft(key, c)))

    cast_return()
  end
end
