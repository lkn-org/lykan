use Lkn.Prelude
import Lkn.Core.System, only: [defsystem: 2]
import Lkn.Core.Component, only: [defcomponent: 2]

defsystem Lykan.System.Appearance do
  defcomponent Component do
    @system Lykan.System.Appearance

    @call get_color() :: String.t
    @cast set_color(c :: String.t)
  end

  @map Component
  @puppet Component

  def init_state(_instance_key, _map_key) do
    :ok
  end

  def puppet_enter(_no_state, _instance_key, map_key, _puppets, key) do
    c = Component.get_color(map_key)
    Component.set_color(key, c)

    notify(&Lykan.Puppeteer.notify(&1, {:puppet_color, key, c}))
    :ok
  end

  def puppet_leave(_no_state, _instance_key, _map_key, _puppets, _key) do
    :ok
  end

  cast change_puppet_color(key :: Lkn.Core.Puppet.k, c :: String.t) do
    Component.set_color(key, c)

    notify(&Lykan.Puppeteer.notify(&1, {:puppet_color, key, c}))

    state
  end
end
