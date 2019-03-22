defmodule OpencensusCowboy.Mixfile do
  use Mix.Project

  def project do
    [app: :opencensus_cowboy,
     version: "0.3.0",
     deps: deps(),
     description: description(),
     package: package()]
  end

  defp description do
    """
    Opencensus instrumenters and context for Cowboy 2.
    """
  end

  defp package do
    [maintainers: ["Ilya Khaprov"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/deadtrickster/opencensus-cowboy"},
     files: ["src", "lib", "README.md", "rebar.config"]]
  end

  defp deps do
    [{:opencensus, "~> 0.9"}]
  end
end
