package sql

import (
	"fmt"
	. "github.com/dave/jennifer/jen"
	"regexp"
	"strings"
)

var proxyName = map[string]string{
	"~": "tilde",
	"!": "examination",
	"@": "at",
	"$": "dollar",
	"%": "percent",
	"^": "carat",
	"&": "ampersand",
	"*": "asterisk",
	"(": "parenthesis_open",
	")": "parenthesis_close",
	"-": "minus",
	"+": "plus",
	"_": "underscore",
	"=": "equal",
	"{": "brace_open",
	"}": "brace_close",
	"[": "bracket_open",
	"]": "bracket_close",
	"|": "bar",
	"\\": "backslash",
	":": "colon",
	";": "semicolon",
	"'": "single_quote",
	"\"": "double_quote",
	",": "comma",
	".": "period",
	"<": "angle_open",
	">": "angle_close",
	"`": "backtick",
	"/": "slash",
}

var isPunctuation = regexp.MustCompile("^[^a-zA-Z0-9]+$")

func toPublicId(fullname string) string {
	result := ""
	for _, token := range strings.Split(fullname, ".") {
		if isPunctuation.MatchString(token) {
			for i := range token {
				result += toPublicId(proxyName[token[i:i+1]])
			}
		} else {
			for _, subtoken := range strings.Split(token, "_") {
				result = result + strings.Title(subtoken)
			}
		}
	}
	return result
}

type StringSet []string

func (s StringSet) Add(e string) StringSet {
	for i := range s {
		if s[i] == e {
			return s
		}
	}
	return append(s, e)
}

func RuleStructs(name string, t *FieldType, names map[string]string) []Code {
	if t.IsStruct() {
		fields := make([]Code, 0)
		for key, value := range *t.Dealias().Struct {
			if value.IsRule() {
				fields = append(fields, Id(toPublicId(key)).Id(names[(*value.Dealias().Rule).Name]))
			} else {
				fields = append(fields, Id(toPublicId(key)).Interface())
			}
		}
		return []Code{Type().Id(names[name]).Struct(fields...)}
	} else if t.IsArray() {
		if t.Dealias().Array.Dealias().IsRule() {
			return []Code{Type().Id(names[name]).Index().Id(names[(*t.Dealias().Array.Dealias().Rule).Name])}
		} else {
			return []Code{Type().Id(names[name]).Index().Interface()}
		}
	} else if t.IsSum() {
		defs := make([]Code, 0)
		fields := make([]Code, 0)
		for _, value := range *t.Sum {
			name := toPublicId(value.Name)
			fields = append(fields, Op("*").Id(name))
			defs = append(defs, RuleStructs(name, value, names)...)
		}
		return append(defs, Type().Id(names[name]).Struct(fields...))
	} else if t.IsRule() {
		return nil
	}
	panic("implement me")
}

func Generate(g *Grammar, packageName string) (string, error) {
	f := NewFile(packageName)

	lexNames := make(map[string]string)
	lexValues := make(map[string]int)
	lexDefs := make([]Code, 0)
	for i, lex := range g.Lexes.Flatten() {
		lexNames[lex.FullName] = "Lex" + toPublicId(lex.FullName)
		lexValues[lex.FullName] = i
		def := Id(lexNames[lex.FullName]).Op("=").Lit(lexValues[lex.FullName])
		lexDefs = append(lexDefs, def)
	}

	f.Const().Defs(lexDefs...)
	f.Line()

	ruleNames := make(map[string]string)
	for _, rule := range g.Rules {
		ruleNames[rule.Name] = toPublicId(rule.Name)
	}

	for _, rule := range g.Rules {
		for _, code := range RuleStructs(rule.Name, &rule.Type, ruleNames) {
			f.Add(code)
		}
	}

	fmt.Printf("%#v", f)

	panic("")
}