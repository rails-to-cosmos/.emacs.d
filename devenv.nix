{ pkgs, lib, config, inputs, ... }:

{
  packages = with pkgs; [fzf fd];
}
