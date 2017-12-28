defmodule Renaissance.Mixfile do
  use Mix.Project

  def project do
    [
      app:              :renaissance,
      version:          "0.1.0",
      elixir:           "~> 1.5",
      start_permanent:  Mix.env == :prod,
      deps:             deps(),
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      mod: {Renaissance, []},
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
      {:lkn_core, "~> 0.1.0"},
      {:lkn_prelude, "~> 0.1.2"},
      {:beacon, "~> 1.1"},
      {:socket, "~> 0.3"},
      {:poison, "~> 3.1.0"},

      # development
      {:ex_doc, "~> 0.16", only: :dev, runtime: false},
    ]
  end
end
