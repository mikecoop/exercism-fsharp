module Grep

open System.IO

let grep files flagArguments pattern =
    Directory.EnumerateFiles(".")