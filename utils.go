package sql

const indentString = "    "

func indent(tokens []string) []string {
    newTokens := make([]string, 0)
    newTokens = append(newTokens, indentString)
    for _, token := range tokens {
        newTokens = append(newTokens, token)
        if token == "\n" {
            newTokens = append(newTokens, indentString)
        }
    }
    return newTokens
}
