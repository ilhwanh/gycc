package sql

import "testing"

func TestNewGrammar(t *testing.T) {
    grammar, err := NewGrammar("grammar.yaml")
    if err != nil {
        t.Error(err)
    }

    tokens, err := grammar.Lexer("select * from a")
    if err != nil {
        t.Error(err)
    }

    t.Logf("tokens: %+v", tokens)
}