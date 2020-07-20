package sql

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

type FieldTypeCanonical int
const (
	FieldTypeInt = FieldTypeCanonical(iota)
	FieldTypeString = FieldTypeCanonical(iota)
	FieldTypeBool = FieldTypeCanonical(iota)
	FieldTypeAny = FieldTypeCanonical(iota)
)
type FieldTypeRule *Rule
type FieldTypeSum []*FieldType
type FieldTypeAnd []*FieldType
type FieldTypeObject struct {
	Key *FieldType
	Value *FieldType
}
type FieldTypeStruct map[string]*FieldType
type FieldTypeLookup struct {
	Object *FieldType
	Key string
}
type FieldType struct {
	Name string
	Rule *FieldTypeRule
	Canonical *FieldTypeCanonical
	Ref *FieldType
	Alias *FieldType
	Array *FieldType
	Sum *FieldTypeSum
	And *FieldTypeAnd
	Object *FieldTypeObject
	Struct *FieldTypeStruct
	Lookup *FieldTypeLookup
}

func (f *FieldType) IsRule() bool {
	return f.Dealias().Rule != nil
}

func (f *FieldType) IsCanonical() bool {
	return f.Dealias().Canonical != nil
}

func (f *FieldType) IsRef() bool {
	return f.Dealias().Ref != nil
}

func (f *FieldType) IsAlias() bool {
	return f.Alias != nil
}

func (f *FieldType) IsArray() bool {
	return f.Dealias().Array != nil
}

func (f *FieldType) IsSum() bool {
	return f.Dealias().Sum != nil
}

func (f *FieldType) IsAnd() bool {
	return f.Dealias().And != nil
}

func (f *FieldType) IsObject() bool {
	return f.Dealias().Object != nil
}

func (f *FieldType) IsStruct() bool {
	return f.Dealias().Struct != nil
}

func (f *FieldType) IsLookup() bool {
	return f.Dealias().Lookup != nil
}

func (f *FieldType) Dealias() *FieldType {
	if f.IsAlias() {
		return f.Alias.Dealias()
	}
	return f
}

func (f *FieldType) Equals(other *FieldType, stack [][2]*FieldType) bool {
	for _, elem := range stack {
		if f == elem[0] && other == elem[1] {
			return true
		}
	}

	left := f.Dealias()
	right := other.Dealias()
	if left.IsRule() && right.IsRule() {
		return left.Rule == right.Rule
	} else if left.IsCanonical() && right.IsCanonical() {
		return *left.Canonical == *right.Canonical
	} else if left.IsRef() && right.IsRef() {
		return left.Ref.Equals(right.Ref, append(stack, [2]*FieldType{f, other}))
	} else if left.IsArray() && right.IsArray() {
		return left.Array.Equals(right.Array, append(stack, [2]*FieldType{f, other}))
	} else if left.IsSum() && right.IsSum() {
		for _, elemLeft := range *left.Sum {
			for _, elemRight := range *right.Sum {
				if elemLeft.Equals(elemRight, append(stack, [2]*FieldType{f, other})) {
					goto Match
				}
			}
			return false
			Match:
		}
		return true
	} else if left.IsAnd() && right.IsAnd() {
		panic("implement me")
	} else if left.IsObject() && right.IsObject() {
		eqKey := left.Object.Key.Equals(right.Object.Key, append(stack, [2]*FieldType{f, other}))
		eqValue := left.Object.Value.Equals(right.Object.Value, append(stack, [2]*FieldType{f, other}))
		return eqKey && eqValue
	} else if left.IsStruct() && right.IsStruct() {
		if len(*left.Struct) != len(*right.Struct) {
			return false
		}
		for key, elemLeft := range *left.Struct {
			elemRight, ok := (*right.Struct)[key]
			if !ok {
				return false
			}
			if !elemLeft.Equals(elemRight, append(stack, [2]*FieldType{f, other})) {
				return false
			}
		}
		return true
	}
	return false
}

func (f *FieldType) Merge(other *FieldType) (*FieldType, error) {
	if !f.IsStruct() || !other.IsStruct() {
		return nil, fmt.Errorf("types must be structs")
	}
	newType := FieldType{Struct: &FieldTypeStruct{}}
	for key, value := range *f.Struct {
		(*newType.Struct)[key] = value
	}
	for key, value := range *other.Struct {
		(*newType.Struct)[key] = value
	}
	return &newType, nil
}

func (f *FieldType) Get(key string) *FieldType {
	return &FieldType{Lookup: &FieldTypeLookup{
		Object: f,
		Key:    key,
	}}
}

type FieldNotationIndex int
type FieldNotationArray []FieldNotation
type FieldNotationLookup struct {
	Object *FieldNotation
	Key string
}
type FieldNotationObject map[string]FieldNotation
type FieldNotationConcat [2]FieldNotation
type FieldNotation struct {
	Literal *string
	Index *FieldNotationIndex
	Array *FieldNotationArray
	Lookup *FieldNotationLookup
	Object *FieldNotationObject
	Concat *FieldNotationConcat
	Type FieldType
}

func (f *FieldNotation) IsLiteral() bool {
	return f.Literal != nil
}

func (f *FieldNotation) IsIndex() bool {
	return f.Index != nil
}

func (f *FieldNotation) IsArray() bool {
	return f.Array != nil
}

func (f *FieldNotation) IsLookup() bool {
	return f.Lookup != nil
}

func (f *FieldNotation) IsObject() bool {
	return f.Object != nil
}

func (f *FieldNotation) IsConcat() bool {
	return f.Concat != nil
}

func ExtractTypeOfNode(node GrammarNode) (*FieldType, error) {
	switch node.(type) {
	case *Wildcard: {
		canonical := FieldTypeAny
		return &FieldType{Canonical: &canonical}, nil
	}
	case *Literal: {
		canonical := FieldTypeString
		return &FieldType{Canonical: &canonical}, nil
	}
	case *Lex: {
		canonical := FieldTypeString
		return &FieldType{Canonical: &canonical}, nil
	}
	case *Rule: {
		rule := FieldTypeRule(node.(*Rule))
		return &FieldType{Rule: &rule}, nil
	}
	default: {
		return nil, fmt.Errorf("not implemented type '%T'", node)
	}
	}
}

func (f *FieldNotation) InferType(parent *Grammar, elements []*RuleSpecElem) error {
	if f.IsLiteral() {
		canonical := FieldTypeString
		f.Type = FieldType{Canonical: &canonical}
		return nil
	} else if f.IsIndex() {
		target := elements[*f.Index].Node
		t, err := ExtractTypeOfNode(target)
		if err != nil {
			return err
		}
		f.Type = *t
		return nil
	} else if f.IsArray() {
		var t *FieldType
		for _, elem := range *f.Array {
			err := elem.InferType(parent, elements)
			if err != nil {
				return err
			}
			if t == nil {
				t = &elem.Type
			} else {
				if !t.Equals(&elem.Type, nil) {
					return fmt.Errorf("types must be the same")
				}
			}
		}
		f.Type = FieldType{Array: t}
		return nil
	} else if f.IsLookup() {
		err := f.Lookup.Object.InferType(parent, elements)
		if err != nil {
			return err
		}
		t := f.Lookup.Object.Type.Get(f.Lookup.Key)
		f.Type = *t
		return nil
	} else if f.IsObject() {
		t := &FieldType{Struct: &FieldTypeStruct{}}
		for key, value := range *f.Object {
			err := value.InferType(parent, elements)
			if err != nil {
				return err
			}
			(*t.Struct)[key] = &value.Type
		}
		f.Type = *t
		return nil
	} else if f.IsConcat() {
		err := f.Concat[0].InferType(parent, elements)
		if err != nil {
			return err
		}
		err = f.Concat[1].InferType(parent, elements)
		if err != nil {
			return err
		}
		// TODO: recursive tolerating type check
		//if !f.Concat[0].Type.Equals(&f.Concat[1].Type, nil) {
		//	return fmt.Errorf("types must be the same")
		//}
		f.Type = FieldType{And: &FieldTypeAnd{&f.Concat[0].Type, &f.Concat[1].Type}}
		return nil
	}
	return fmt.Errorf("unknown type of notation")
}

func (r *Rule) InferType(parent *Grammar) error {
	r.SubTypes = make(map[string]FieldType)

	for _, sub := range r.Subs {
		err := sub.FieldNotation.InferType(parent, sub.SpecElements)
		if err != nil {
			return err
		}
		subType, ok := r.SubTypes[sub.Type]
		if !ok {
			r.SubTypes[sub.Type] = sub.FieldNotation.Type
		} else {
			if subType.IsStruct() {
				newSubType, err := subType.Merge(&sub.FieldNotation.Type)
				if err != nil {
					return err
				}
				r.SubTypes[sub.Type] = *newSubType
			}
		}
	}

	sum := FieldTypeSum{}
	for key, value := range r.SubTypes {
		value.Name = key
		sum = append(sum, &value)
	}

	if len(sum) == 1 {
		r.Type = FieldType{Alias: sum[0]}
	} else {
		r.Type = FieldType{Sum: &sum}
	}

	return nil
}

func NewFieldNotation(parent *Grammar, raw interface{}) (*FieldNotation, error) {
	switch raw.(type) {
	case int: {
		index := FieldNotationIndex(raw.(int))
		return &FieldNotation{Index: &index}, nil
	}
	case string: {
		rep := raw.(string)
		rep = strings.TrimSpace(rep)

		patternLiteral := regexp.MustCompile("`(.*)`")
		if matches := patternLiteral.FindStringSubmatch(rep); matches != nil {
			return &FieldNotation{Literal: &matches[1]}, nil
		}

		patternIndex := regexp.MustCompile(`^\d+$`)
		if matches := patternIndex.FindStringSubmatch(rep); matches != nil {
			index, err := strconv.Atoi(matches[0])
			if err != nil {
				return nil, err
			}
			return NewFieldNotation(parent, index)
		}

		patternArray := regexp.MustCompile(`^\[(.*)]$`)
		if matches := patternArray.FindStringSubmatch(rep); matches != nil {
			array := strings.Split(matches[1], ",")
			erased := make([]interface{}, len(array))
			for i, elem := range array {
				erased[i] = elem
			}
			return NewFieldNotation(parent, erased)
		}

		patternConcat := regexp.MustCompile(`^(.+?)\s*\+\s*(.+)$`)
		if matches := patternConcat.FindStringSubmatch(rep); matches != nil {
			head, err := NewFieldNotation(parent, matches[1])
			if err != nil {
				return nil, err
			}

			tail, err := NewFieldNotation(parent, matches[2])
			if err != nil {
				return nil, err
			}

			return &FieldNotation{Concat: &FieldNotationConcat{*head, *tail}}, nil
		}

		patternLookup := regexp.MustCompile(`^(.+)\.(.+?)$`)
		if matches := patternLookup.FindStringSubmatch(rep); matches != nil {
			object, err := NewFieldNotation(parent, matches[1])
			if err != nil {
				return nil, err
			}
			key := matches[2]

			return &FieldNotation{Lookup: &FieldNotationLookup{
				Object: object,
				Key:    key,
			}}, nil
		}

		return nil, fmt.Errorf("unrecognized representation '%s'", rep)
	}
	case []interface{}: {
		raws := raw.([]interface{})

		notations := make(FieldNotationArray, len(raws))
		for i, elem := range raws {
			notation, err := NewFieldNotation(parent, elem)
			if err != nil {
				return nil, err
			}
			notations[i] = *notation
		}

		return &FieldNotation{Array: &notations}, nil
	}
	case map[string]interface{}: {
		rawMap := raw.(map[string]interface{})

		object := make(FieldNotationObject)
		for key, elem := range rawMap {
			field, err := NewFieldNotation(parent, elem)
			if err != nil {
				return nil, err
			}
			object[key] = *field
		}

		return &FieldNotation{Object: &object}, nil
	}
	}

	return nil, fmt.Errorf("cannot recognize factory pattern '%+v' (%T)", raw, raw)
}
