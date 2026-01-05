defmodule BPF.MixProject do
  use Mix.Project

  @version "0.1.1"
  @source_url "https://github.com/ausimian/bpf"

  def project do
    [
      app: :bpf,
      version: @version,
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: package(),
      description: description(),

      # Test coverage
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test,
        "coveralls.github": :test
      ],

      # Docs
      name: "BPF",
      source_url: @source_url,
      homepage_url: @source_url,
      docs: [
        main: "BPF",
        extras: ["README.md", "LICENSE.md"],
        source_ref: @version
      ]
    ]
  end

  defp description do
    """
    Compile Elixir binary pattern matching to classic BPF bytecode.

    Write packet filters using familiar Elixir syntax with binary patterns and guards,
    and compile them to BPF programs for use with sockets or libpcap.
    """
  end

  defp package do
    [
      name: "bpf",
      licenses: ["MIT"],
      links: %{
        "GitHub" => @source_url
      },
      files: ~w(lib .formatter.exs mix.exs README.md LICENSE.md)
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:ex_doc, "~> 0.39", only: :dev, runtime: false},
      {:excoveralls, "~> 0.18", only: :test},
      {:castore, "~> 1.0", only: :test}
    ]
  end
end
