{
  description = "some nix flake templates for haskell development";

  outputs = { self, nixpkgs }: {

    defaultTemplate.path = ./haskell-hello;
    defaultTemplate.description = "hello world in haskell with bloated developments tools";
  };
}
