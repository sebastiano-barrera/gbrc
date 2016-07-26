package main

import (
	"os"
	"flag"
	"fmt"
	"gbrc/parser"
	"gbrc/backend"
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

	pars := parser.NewParser(romFile)
	err = comp.Parse()
	if err != nil {
		fmt.Fprintf(os.Stderr, "compile error: %s\n", err)
		return
	}
}
