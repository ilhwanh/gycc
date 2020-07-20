package sql

import (
    "bytes"
    "fmt"
    "gopkg.in/yaml.v3"
    "io/ioutil"
    "regexp"
    "strings"
)

type GrammarNode interface {
    Find(tokens []string) GrammarNode
}

type Literal struct {
    Value string
}

func (l *Literal) Find(tokens []string) GrammarNode {
    return nil
}

type Wildcard struct {}

func (w *Wildcard) Find(tokens []string) GrammarNode {
    return nil
}

type Epsilon struct {}

func (e *Epsilon) Find(tokens []string) GrammarNode {
    return nil
}

func IsEpsilon(v interface{}) bool {
    if _, ok := v.(*Epsilon); ok {
        return true
    }
    return false
}

type Precedence []int

func (p Precedence) Less(other Precedence) bool {
    length := len(p)
    if len(other) < length {
        length = len(other)
    }
    for i := 0; i < length; i++ {
        if p[i] == other[i] {
            continue
        }
        return p[i] < other[i]
    }
    if len(p) < len(other) {
        return true
    }
    return false
}

func (p Precedence) Equals(other Precedence) bool {
    if len(p) != len(other) {
        return false
    }
    for i := range p {
        if p[i] != other[i] {
            return false
        }
    }
    return true
}

type Lex struct {
    FullName string `yaml:"-"`
    Name string `yaml:"name"`
    Elements Lexes `yaml:"elements"`
    Break bool `yaml:"break"`
    Insensitive bool `yaml:"insensitive"`
    Keyword []string `yaml:"keyword"`
    Regex string `yaml:"regex"`
    Pattern *regexp.Regexp `yaml:"-"`
    Precedence `yaml:"-"`
}

func (l *Lex) Init(parent *Lex, precedence Precedence) error {
    if parent != nil {
        l.FullName = fmt.Sprintf("%s.%s", parent.FullName, l.Name)
        l.Break = parent.Break || l.Break
        l.Insensitive = parent.Insensitive || l.Insensitive
    } else {
        l.FullName = l.Name
    }
    l.Precedence = precedence

    if l.Keyword != nil {
        for _, keyword := range l.Keyword {
            l.Elements = append(l.Elements, &Lex{
                Name: keyword,
            })
        }
        l.Keyword = nil
    }

    if l.Elements != nil {
        if err := l.Elements.Init(l); err != nil {
            return err
        }
    }

    if l.Regex != "" && l.Pattern == nil {
        pattern, err := regexp.Compile(l.Regex)
        if err != nil {
            return err
        }
        l.Pattern = pattern
    }

    return nil
}

func (l *Lex) Find(tokens []string) GrammarNode {
    if tokens[0] == l.Name {
        if len(tokens) == 1 {
            return l
        } else {
            if found := l.Elements.Find(tokens[1:]); found != nil {
                return found
            }
        }
    }
    return nil
}

func (l *Lex) Match(s string) (TokenSet, bool) {
    if l.Elements != nil {
        return l.Elements.Match(s)
    }

    if l.Pattern != nil {
        loc := l.Pattern.FindStringIndex(s)
        if loc == nil {
            return nil, true
        }
        if loc[0] != 0 {
            return nil, true
        }
        return TokenSet{
            {
                Text: s[loc[0]:loc[1]],
                Start: loc[0],
                End: loc[1],
                Lex: l,
            },
        }, !l.Break
    }

    if strings.Index(s, l.Name) == 0 || (l.Insensitive && strings.Index(strings.ToLower(s), strings.ToLower(l.Name)) == 0) {
        return TokenSet{
            {
                Text: s[:len(l.Name)],
                Start: 0,
                End: len(l.Name),
                Lex: l,
            },
        }, !l.Break
    }

    return nil, true
}

func (l *Lex) IsDescendant(other *Lex) bool {
    if other == nil {
        return false
    }
    if l == other {
        return true
    }
    for _, child := range l.Elements {
        if child.IsDescendant(other) {
            return true
        }
    }
    return false
}

func (l *Lex) Flatten() Lexes {
    lexes := Lexes{}
    if l.Elements != nil {
        return l.Elements.Flatten()
    } else {
        lexes = append(lexes, l)
    }
    return lexes
}

type Lexes []*Lex

func (l Lexes) Init(parent *Lex) error {
    var precedence Precedence
    if parent != nil {
        precedence = parent.Precedence
    } else {
        precedence = Precedence{}
    }
    for i, lex := range l {
        if err := lex.Init(parent, append(precedence, i)); err != nil {
            return err
        }
    }
    return nil
}

func (l Lexes) Find(tokens []string) GrammarNode {
    for _, lex := range l {
        if found := lex.Find(tokens); found != nil {
            return found
        }
    }
    return nil
}

func (l Lexes) Match(s string) (TokenSet, bool) {
    cont := true
    tokenSet := TokenSet{}
    for _, lex := range l {
        matches, localCont := lex.Match(s)
        tokenSet = tokenSet.Add(matches...)
        if len(matches) > 0 && localCont {
            cont = localCont
            break
        }
    }
    return tokenSet, cont
}

func (l Lexes) Flatten() Lexes {
    lexes := Lexes{}
    for _, lex := range l {
        lexes = append(lexes, lex.Flatten()...)
    }
    return lexes
}

type Flag int

const (
    FlagMany = Flag(iota)
    FlagOneOrMore = Flag(iota)
    FlagMaybe = Flag(iota)
    FlagLookAhead = Flag(iota)
    FlagLookBehind = Flag(iota)
)

type Flags []Flag

func NewFlags(rep string) (Flags, error) {
    flags := Flags{}
    if strings.Contains(rep, "*") {
        flags = append(flags, FlagMany)
    }
    if strings.Contains(rep, "+") {
        flags = append(flags, FlagOneOrMore)
    }
    if strings.Contains(rep, "?") {
        flags = append(flags, FlagMaybe)
    }
    if strings.Contains(rep, "<") {
        flags = append(flags, FlagLookAhead)
    }
    if strings.Contains(rep, ">") {
        flags = append(flags, FlagLookBehind)
    }
    return flags, nil
}

func (f Flags) Check(target Flag) bool {
    for _, flag := range f {
        if flag == target {
            return true
        }
    }
    return false
}

func (f Flags) IsEmpty() bool {
    return len(f) == 0
}

type RuleSpecElem struct {
    Node GrammarNode
    Flags
}

func NewRuleSpecElem(parent *Grammar, rep string) (*RuleSpecElem, error) {
    pattern := regexp.MustCompile("^(?P<Path>.*?)(\\[(?P<Flags>.*)])?$")
    match := pattern.FindStringSubmatch(rep)
    result := make(map[string]string)
    for i, name := range pattern.SubexpNames() {
        if i != 0 && name != "" {
            result[name] = match[i]
        }
    }

    node, err := parent.Resolve(result["Path"])
    if err != nil {
        return nil, err
    }

    flags, err := NewFlags(result["Flags"])
    if err != nil {
        return nil, err
    }

    return &RuleSpecElem{
        Node:  node,
        Flags: flags,
    }, nil
}

type RuleSpec struct {
    Head *RuleSpecNode
    Nodes []*RuleSpecNode
}

func NewRuleSpec(parent *Grammar, reps []string) (*RuleSpec, []*RuleSpecElem, error) {
    prev := &RuleSpecNode{}
    spec := RuleSpec{Head: prev, Nodes: []*RuleSpecNode{prev}}
    elements := make([]*RuleSpecElem, 0)

    for _, rep := range reps {
        elem, err := NewRuleSpecElem(parent, rep)
        if err != nil {
            return nil, nil, err
        }
        elements = append(elements, elem)

        next := &RuleSpecNode{}
        spec.Nodes = append(spec.Nodes, next)

        if elem.Flags.Check(FlagMaybe) || elem.Flags.Check(FlagMany) {
            prev.Add(RuleSpecEdge{
                Transition: &Epsilon{},
                To:         next,
                Consume:    false,
            })
        }

        if elem.Flags.Check(FlagMany) || elem.Flags.Check(FlagOneOrMore) {
            prev.Add(RuleSpecEdge{
                Transition: elem.Node,
                To:         prev,
                Consume:    !(elem.Flags.Check(FlagLookAhead) || elem.Flags.Check(FlagLookBehind)),
            })
        }

        prev.Add(RuleSpecEdge{
            Transition: elem.Node,
            To:         next,
            Consume:    !(elem.Flags.Check(FlagLookAhead) || elem.Flags.Check(FlagLookBehind)),
        })

        prev = next
    }

    spec.EliminateEpsilonMoves()

    return &spec, elements, nil
}

func (r RuleSpec) NoIncomingEpsilon() []*RuleSpecNode {
    nodes := make([]*RuleSpecNode, 0)
    for _, node := range r.Nodes {
        incoming := false
        for _, prev := range node.Prev {
            if IsEpsilon(prev.Transition) {
                incoming = true
                break
            }
        }
        if !incoming {
            nodes = append(nodes, node)
        }
    }
    return nodes
}

func (r *RuleSpec) EliminateEpsilonMoves() {
    for {
        nodes := r.NoIncomingEpsilon()
        if len(nodes) == 0 {
            panic("cyclic epsilon moves")
        }

        if len(nodes) == len(r.Nodes) {
            break
        }

        for _, node := range nodes {
            for _, next := range node.Next {
                if IsEpsilon(next.Transition) {
                    for _, prev := range node.Prev {
                        otherNext := node.GetNext(&prev)
                        newOtherNext := RuleSpecEdge{
                            Transition: otherNext.Transition,
                            To:         next.To,
                            Consume:    otherNext.Consume,
                        }
                        prev.To.Add(newOtherNext)
                    }
                    node.Remove(&next)
                    goto SafeContinue
                }
            }
        }

        panic("cyclic epsilon moves")

    SafeContinue:
        continue
    }
}

type RuleSpecNode struct {
    Next []RuleSpecEdge
    Prev []RuleSpecEdge
}

func (r *RuleSpecNode) Add(edge RuleSpecEdge) {
    r.Next = append(r.Next, edge)
    edge.To.Prev = append(edge.To.Prev, RuleSpecEdge{
        Transition: edge.Transition,
        To:         r,
        Consume:    edge.Consume,
    })
}

func (r *RuleSpecNode) Remove(edge *RuleSpecEdge) {
    newNext := make([]RuleSpecEdge, 0)
    for _, next := range r.Next {
        if !next.Equals(*edge) {
            newNext = append(newNext, next)
        }
    }
    r.Next = newNext

    other := edge.To
    otherEdge := r.GetPrev(edge)
    newPrev := make([]RuleSpecEdge, 0)
    for _, prev := range other.Prev {
        if !prev.Equals(*otherEdge) {
            newPrev = append(newPrev, prev)
        }
    }
    other.Prev = newPrev
}

func (r *RuleSpecNode) GetPrev(edge *RuleSpecEdge) *RuleSpecEdge {
    other := edge.To
    for _, prev := range other.Prev {
        if prev.To == r {
            return &prev
        }
    }
    return nil
}

func (r *RuleSpecNode) GetNext(edge *RuleSpecEdge) *RuleSpecEdge {
    other := edge.To
    for _, next := range other.Next {
        if next.To == r {
            return &next
        }
    }
    return nil
}

type RuleSpecEdge struct {
    Transition GrammarNode
    To *RuleSpecNode
    Consume bool
}

func (r RuleSpecEdge) Equals(other RuleSpecEdge) bool {
    return r.Transition == other.Transition && r.To == other.To && r.Consume == other.Consume
}

type ConsumerState int

const (
    ConsumerStateLookAhead = ConsumerState(iota)
    ConsumerStateConsuming = ConsumerState(iota)
    ConsumerStateLookBehind = ConsumerState(iota)
)

type RuleSpecMatchState struct {
    Current         *RuleSpecNode
    Matches         []*Token
    ConsumeStart    int
    ConsumeEnd      int
    EffectiveLength int
    ConsumerState
}

func NewRuleSpecMatchState(parent *RuleSpec) RuleSpecMatchState {
    return RuleSpecMatchState{
        Current:         parent.Head,
        Matches:         []*Token{},
        ConsumeStart:    0,
        ConsumeEnd:      0,
        EffectiveLength: 0,
        ConsumerState:   ConsumerStateLookAhead,
    }
}

func (r RuleSpecMatchState) IsFinal() bool {
    return len(r.Current.Next) == 0
}

func (r RuleSpecMatchState) HasEpsilonMove() bool {
    for _, edge := range r.Current.Next {
        if IsEpsilon(edge.Transition) {
            return true
        }
    }
    return false
}

func (r RuleSpecMatchState) Step(tokenSet *TokenSet) []RuleSpecMatchState {
    newStates := make([]RuleSpecMatchState, 0)
    for _, edge := range r.Current.Next {
        matched := tokenSet.Match(edge.Transition)
        if matched != nil {
            newState := RuleSpecMatchState{
                Matches: append(r.Matches, matched),
                ConsumerState: r.ConsumerState,
                EffectiveLength: r.EffectiveLength,
            }

            if matched.IsWhitespace() {
                newState.Current = r.Current
                switch newState.ConsumerState {
                case ConsumerStateLookAhead: {
                    newState.ConsumeStart = r.ConsumeStart + 1
                    newState.ConsumeEnd = r.ConsumeEnd
                }
                case ConsumerStateConsuming: {
                    newState.ConsumeStart = r.ConsumeStart
                    newState.ConsumeEnd = r.ConsumeEnd
                }
                case ConsumerStateLookBehind: {
                    newState.ConsumeStart = r.ConsumeStart
                    newState.ConsumeEnd = r.ConsumeEnd - 1
                }
                default: {
                    panic(fmt.Sprintf("illegal consume state: %d", newState.ConsumerState))
                }
                }
            } else {
                newState.Current = edge.To
                if edge.Consume {
                    if newState.ConsumerState == ConsumerStateLookAhead || newState.ConsumerState == ConsumerStateConsuming {
                        newState.ConsumeStart = r.ConsumeStart
                        newState.ConsumeEnd = r.ConsumeEnd
                        newState.ConsumerState = ConsumerStateConsuming
                        newState.EffectiveLength += 1
                    } else {
                        panic("illegal consume state")
                    }
                } else {
                    if newState.ConsumerState == ConsumerStateLookAhead {
                        newState.ConsumeStart = r.ConsumeStart + 1
                        newState.ConsumeEnd = r.ConsumeEnd
                    } else {
                        newState.ConsumeStart = r.ConsumeStart
                        newState.ConsumeEnd = r.ConsumeEnd - 1
                        newState.ConsumerState = ConsumerStateLookBehind
                    }
                }
            }

            newStates = append(newStates, newState)
        }
    }
    return newStates
}

type RuleSpecMatchSub struct {
    *Rule
    Elements []*Token
}

func (r RuleSpecMatchSub) IsAliasing() bool {
    return len(r.Elements) == 1
}

type RuleSpecMatch struct {
    Valid bool
    Elements []RuleSpecMatchSub
    Span [2]int
    Consume [2]int
}

func (r RuleSpecMatch) IsAliasing() bool {
    return r.Valid && len(r.Elements) >= 1 && r.Elements[0].IsAliasing()
}

func NewRuleSpecMatchEmpty() *RuleSpecMatch {
    return &RuleSpecMatch{
        Valid: false,
    }
}

func NewRuleSpecMatch(elements []*Token, span [2]int, consume [2]int) *RuleSpecMatch {
    return &RuleSpecMatch{
        Valid: true,
        Elements: []RuleSpecMatchSub{
            {
                Rule: nil,
                Elements: elements,
            },
        },
        Span: span,
        Consume: consume,
    }
}

func (r *RuleSpecMatch) Equals(other *RuleSpecMatch) bool {
    return r.Span == other.Span && r.Consume == other.Consume
}

func (r *RuleSpecMatch) Less(other *RuleSpecMatch) bool {
    return other.Span[0] < r.Span[0] || (other.Span[0] == r.Span[0] && other.Consume[0] < r.Consume[0])
}

func (r *RuleSpecMatch) Update(other *RuleSpecMatch) {
    if other == nil {
        return
    }

    if !r.Valid {
        r.Valid = other.Valid
        r.Elements = other.Elements
        r.Span = other.Span
        r.Consume = other.Consume
        return
    }

    if !r.IsAliasing() && other.IsAliasing() {
        r.Valid = other.Valid
        r.Elements = other.Elements
        r.Span = other.Span
        r.Consume = other.Consume
        return
    }

    if r.Equals(other) {
        r.Elements = append(r.Elements, other.Elements...)
        return
    }
}

func (r *RuleSpecMatch) Mark(rule *Rule) {
    for i := range r.Elements {
        r.Elements[i].Rule = rule
    }
}

func (r RuleSpec) Match(lexed Lexed, reversed bool) *RuleSpecMatch {
    if reversed {
        return r.MatchRight(lexed)
    } else {
        return r.MatchLeft(lexed)
    }
}

func (r RuleSpec) MatchLeft(partial Lexed) *RuleSpecMatch {
    var longest *RuleSpecMatchState
    states := []RuleSpecMatchState{NewRuleSpecMatchState(&r)}

    for _, tokenSet := range partial {
        newStates := make([]RuleSpecMatchState, 0)
        for _, state := range states {
            newPartialStates := state.Step(&tokenSet)
            newStates = append(newStates, newPartialStates...)
        }
        if len(newStates) == 0 {
            break
        }
        states = newStates

        for _, state := range states {
            if state.IsFinal() && state.EffectiveLength > 0 {
                if longest == nil || state.EffectiveLength > longest.EffectiveLength {
                    longest = &state
                }
            }
        }
    }

    if longest != nil {
        return NewRuleSpecMatch(
            longest.Matches[longest.ConsumeStart:len(longest.Matches) + longest.ConsumeEnd],
            [2]int{0, len(longest.Matches)},
            [2]int{longest.ConsumeStart, len(longest.Matches) + longest.ConsumeEnd},
        )
    }

    return nil
}

func (r *RuleSpec) MatchRight(lexed Lexed) *RuleSpecMatch {
    panic("implement me")
}

type RuleSub struct {
    SpecRaw []string `yaml:"spec"`
    SpecElements []*RuleSpecElem `yaml:"-"`
    Spec *RuleSpec `yaml:"-"`
    Type string `yaml:"type"`
    MappingRaw interface{} `yaml:"mapping"`
    FieldNotation         `yaml:"-"`
}

func (r *RuleSub) Init(parent *Grammar) error {
    spec, elements, err := NewRuleSpec(parent, r.SpecRaw)
    if err != nil {
        return err
    }

    notation, err := NewFieldNotation(parent, r.MappingRaw)
    if err != nil {
        return err
    }

    r.Spec = spec
    r.SpecElements = elements
    r.FieldNotation = *notation
    return nil
}

type Rule struct {
    Name        string      `yaml:"name"`
    Reversed    bool        `yaml:"reversed"`
    Whitespace  bool        `yaml:"whitespace"`
    Subs        []*RuleSub  `yaml:"rules"`
    Type        FieldType   `yaml:"-"`
    SubTypes    map[string]FieldType `yaml:"-"`
}

func (r *Rule) Init(parent *Grammar) error {
    for _, sub := range r.Subs {
        err := sub.Init(parent)
        if err != nil {
            return err
        }
    }

    err := r.InferType(parent)
    if err != nil {
        return err
    }

    return nil
}

func (r *Rule) Find(tokens []string) GrammarNode {
    if tokens[0] == r.Name {
        if len(tokens) == 1 {
            return r
        }
    }
    return nil
}

func (r *Rule) Match(lexed Lexed) *RuleSpecMatch {
    match := NewRuleSpecMatchEmpty()
    for _, sub := range r.Subs {
        for i := range lexed {
            partial := lexed[i:]
            newMatch := sub.Spec.Match(partial, r.Reversed)
            if newMatch != nil {
                newMatch.Consume[0] += i
                newMatch.Consume[1] += i

                if newMatch.Valid && newMatch.Consume[1] - newMatch.Consume[0] == 1 {
                    aliased := lexed[newMatch.Consume[0]]
                    for _, token := range aliased {
                        if token.Rule == r {
                            newMatch = nil
                            break
                        }
                    }
                }
            }
            match.Update(newMatch)
        }
    }

    match.Mark(r)
    return match
}

type Rules []*Rule

func (r Rules) Init(parent *Grammar) error {
    for _, rule := range r {
        err := rule.Init(parent)
        if err != nil {
            return err
        }
    }

    return nil
}

func (r Rules) Find(tokens []string) GrammarNode {
    for _, rule := range r {
        if found := rule.Find(tokens); found != nil {
            return found
        }
    }
    return nil
}

func (r Rules) Match(lexed Lexed) *RuleSpecMatch {
    match := NewRuleSpecMatchEmpty()
    for _, rule := range r {
        match.Update(rule.Match(lexed))
    }
    return match
}

func (r Rules) Reduce(lexed Lexed) (Lexed, error) {
    match := r.Match(lexed)

    if match.Valid {
        consumed := lexed[match.Consume[0]:match.Consume[1]]
        newTokenSet := make(TokenSet, len(match.Elements))
        for i, elem := range match.Elements {
            newTokenSet[i] = Token{
                Start:    consumed[0][0].Start,
                End:      consumed[len(consumed) - 1][0].End,
                Rule:     elem.Rule,
                Elements: elem.Elements,
            }
        }
        if len(consumed) == 1 {
            original := consumed[0]
            for _, token := range original {
                newTokenSet = append(newTokenSet, token)
            }
        }
        newLexed := lexed[:match.Consume[0]]
        newLexed = append(newLexed, newTokenSet)
        newLexed = append(newLexed, lexed[match.Consume[1]:]...)
        return newLexed, nil
    }

    return lexed, fmt.Errorf("no matching rules")
}

type Grammar struct {
    Lexes `yaml:"lex"`
    Rules `yaml:"rule"`
}

func (g *Grammar) Resolve(rep string) (GrammarNode, error) {
    if rep == "." {
        return &Wildcard{}, nil
    }

    if strings.Index(rep, ".") != 0 {
        return &Literal{
            Value: rep,
        }, nil
    }

    tokens := strings.Split(rep, ".")
    if tokens[1] == "lex" {
        if found := g.Lexes.Find(tokens[2:]); found != nil {
            return found, nil
        }
    } else if tokens[1] == "rule" {
        if found := g.Rules.Find(tokens[2:]); found != nil {
            return found, nil
        }
    }

    return nil, fmt.Errorf("cannot find '%s'", rep)
}

func (g *Grammar) Lexer(s string) (Lexed, error) {
    lexed := Lexed{}
    pointer := 0

    for {
        if pointer == len(s) {
            break
        }

        tokenSet, _ := g.Lexes.Match(s[pointer:])
        if len(tokenSet) == 0 {
            return nil, fmt.Errorf("unexpected character '%s'", s[pointer:pointer + 1])
        }

        tokenSet.Shift(pointer)
        pointer += tokenSet.Length()
        lexed = append(lexed, tokenSet)
    }

    return lexed, nil
}

func (g *Grammar) Step(lexed Lexed) (Lexed, error) {
    lexed, err := g.Rules.Reduce(lexed)
    if err != nil {
        return nil, err
    }

    return lexed, nil
}

func (g *Grammar) CheckSuccess(lexed Lexed) (*ParseResult, error) {
    result := ParseResult{}
    pointer := 0

    for i, tokenSet := range lexed {
        pointer = i
        token := tokenSet.Whitespace()
        if token != nil {
            result.Prefix = append(result.Prefix, token)
        } else {
            result.Tree = &tokenSet[0]
            break
        }
    }

    pointer++

    for i := pointer; i < len(lexed); i++ {
        tokenSet := lexed[i]
        token := tokenSet.Whitespace()
        if token == nil {
            return nil, fmt.Errorf("one or more than non-whitespace left")
        }
        result.Postfix = append(result.Postfix, token)
    }

    return &result, nil
}

func (g *Grammar) Parse(lexed Lexed) (*ParseResult, error) {
    for {
        newLexed, err := g.Rules.Reduce(lexed)
        if err != nil {
            result, err := g.CheckSuccess(newLexed)
            if err != nil {
                return nil, err
            }
            return result, nil
        }
        lexed = newLexed
        printLexed(lexed)
    }
}

type ParseResult struct {
    Prefix  []*Token
    Tree    *Token
    Postfix []*Token
}

type Token struct {
    Text string
    Start int
    End int
    *Lex
    *Rule
    Elements []*Token
}

func (t Token) IsWhitespace() bool {
    if t.Rule != nil && t.Rule.Whitespace {
        return true
    }
    return false
}

type TokenSet []Token

func (t TokenSet) Add(tokens... Token) TokenSet {
    if len(t) == 0 {
        return tokens
    }
    accepted := make([]Token, 0)
    for _, token := range tokens {
        if t[0].Text == token.Text {
            accepted = append(accepted, token)
        }
    }
    return append(t, accepted...)
}

func (t TokenSet) Shift(n int) {
    for i := range t {
        t[i].Start += n
        t[i].End += n
    }
}

func (t TokenSet) Length() int {
    return len(t[0].Text)
}

func (t TokenSet) Match(node GrammarNode) *Token {
    switch node.(type) {
    case *Wildcard: {
        return &t[0]
    }
    case *Literal: {
        lit := node.(*Literal)
        for _, token := range t {
            if lit.Value == token.Text {
                return &token
            }
        }
    }
    case *Lex: {
        lex := node.(*Lex)
        for _, token := range t {
            if lex.IsDescendant(token.Lex) {
                return &token
            }
        }
    }
    case *Rule: {
        rule := node.(*Rule)
        for _, token := range t {
            if rule == token.Rule {
                return &token
            }
        }
    }
    default: {
        panic("implement me")
    }
    }

    return t.Whitespace()
}

func (t TokenSet) Whitespace() *Token {
    for _, token := range t {
        if token.Rule != nil && token.Rule.Whitespace {
            return &token
        }
    }
    return nil
}

type Lexed []TokenSet

func NewGrammar(path string) (*Grammar, error) {
    b, err := ioutil.ReadFile(path)
    if err != nil {
        return nil, err
    }

    var grammar Grammar
    err = yaml.NewDecoder(bytes.NewBuffer(b)).Decode(&grammar)
    if err != nil {
        return nil, err
    }

    err = grammar.Lexes.Init(nil)
    if err != nil {
        return nil, err
    }

    err = grammar.Rules.Init(&grammar)
    if err != nil {
        return nil, err
    }

    return &grammar, nil
}

func printLexed(lexed Lexed) {
    for j, tokenSet := range lexed {
        if j != 0 {
            fmt.Print(", ")
        }
        for i, token := range tokenSet {
            if i != 0 {
                fmt.Print("|")
            }
            if token.Rule != nil {
                fmt.Printf("%+v", token.Rule.Name)
            } else if token.Lex != nil {
                fmt.Printf("%+v", token.Lex.FullName)
            }
        }
    }
    fmt.Println()
}

func PrintTree(token *Token, indent int) {
    fmt.Print(strings.Repeat("  ", indent))
    if token.Rule != nil {
        fmt.Printf("(rule)%s (%d:%d)", token.Rule.Name, token.Start, token.End)
    } else if token.Lex != nil {
        fmt.Printf("(lex)%s: '%s' (%d:%d)", token.Lex.FullName, strings.ReplaceAll(token.Text, "\n", "\\n"), token.Start, token.End)
    }
    fmt.Println()
    for _, child := range token.Elements {
        PrintTree(child, indent + 1)
    }
}