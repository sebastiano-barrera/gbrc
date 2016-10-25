package main

import (
	"os"
	"flag"
	"fmt"
	"gbrc/parser"
	"gbrc/backend/assembly"
)

var (
	romFilename = flag.String("rom", "", "ROM file (usually .gbc)")
)

func main() {
	flag.Parse()

	if *romFilename == "" {
		fmt.Printf("rom filename is required. Try `%v -h`\n", os.Args[0])
		return
	}

	romFile, err := os.Open(*romFilename)
	if err != nil {
		fmt.Fprintf(os.Stderr, "couldn't open rom: %s: %s\n", *romFilename, err)
		return
	}

	blocks, err := parser.Parse(romFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "parse error: %s\n", err)
		return
	}

	err = assembly.Compile(*blocks, os.Stdout)
	if err != nil {
		fmt.Fprintf(os.Stderr, "compile error: %s\n", err)
		return
	}
}
