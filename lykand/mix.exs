defmodule Lykan.Mixfile do
  use Mix.Project

  def project do
    [
      app:              :lykand,
      version:          "0.1.0",
      elixir:           "~> 1.5",
      start_permanent:  Mix.env == :prod,
      deps:             deps(),
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      mod: {Lykan, []},
      extra_applications: [
        :logger,
        :lkn_core,
      ],
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:uuid, "~> 1.1"},
      {:lykan_repo, path: "../lykan-repo"},
      {:lkn_core, "~> 0.4.3"},
      {:lkn_physics, path: "../../lkn-physics"},
      {:lkn_prelude, "~> 0.1.2"},
      {:beacon, "~> 1.1"},
      {:socket, "~> 0.3"},
      {:poison, "~> 3.1.0"},
      {:postgrex, "~> 0.13"},
      {:ecto, "~> 2.1"},

      # development
      {:ex_doc, "~> 0.16", only: :dev, runtime: false},
      {:distillery, "~> 1.5.2", runtime: false}
    ]
  end
end
