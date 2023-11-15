import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

class main {
    static List<String> Tokenization(String input, List<String> v) {
        StringBuilder token = new StringBuilder();

        for (int i = 0; i < input.length(); i++) {
            char ch = input.charAt(i);

            if (Character.isWhitespace(ch)) {
                // Skip whitespaces
                continue;
            } else if (isOperator(String.valueOf(ch)) || isParenthesis(String.valueOf(ch))) {
                // Add operators and parentheses as separate tokens
                if (token.length() > 0) {
                    v.add(token.toString());
                    token.setLength(0); // Reset the token
                }

                // Check for negative numbers
                if (ch == '-' && (i == 0 || isOperator(String.valueOf(input.charAt(i - 1)))
                        || isParenthesis(String.valueOf(input.charAt(i - 1))))) {
                    token.append(ch); // Include '-' in the token for negative numbers
                } else {
                    v.add(String.valueOf(ch));
                }
            } else {
                // Continue building the value token
                token.append(ch);
            }
        }

        // Add the last token if any
        if (token.length() > 0) {
            v.add(token.toString());
        }

        return v;
    }

    // Function to check if the current token is a ( or )
    static boolean isParenthesis(String ch) {
        return ch.equals("(") || ch.equals(")");
    }

    // check if the given string is an operator or not
    static boolean isOperator(String ch) {
        if (ch.length() > 1)
            return false;
        return ch.equals("+") || ch.equals("-") || ch.equals("*") || ch.equals("/");
    }

    // check if the given substring is a number/decimal number or not
    static boolean isNumber(String str) {
        int len = str.length();
        if (len == 0) {
            return false;
        }

        int i = 0;

        // Allow a '-' sign at the beginning
        if (str.charAt(0) == '-') {
            i = 1;
            // Check if there is only a '-' sign
            if (len == 1) {
                return false;
            }
        }
        int numOfDecimal = 0;
        for (; i < len; i++) {
            if (str.charAt(i) == '.') {
                numOfDecimal++;
            } else if (str.charAt(i) > '9' || str.charAt(i) < '0') {
                return false;
            }

            if ((str.charAt(i) > '9' || str.charAt(i) < '0') && str.charAt(i) != '.') {
                return false;
            }
        }

        if (numOfDecimal > 1 || str.charAt(0) == '.' || str.charAt(len - 1) == '.') {
            return false;
        }

        return true;
    }

    // Function to parse an expression based on the grammar rules
    static boolean expr(List<String> tokens, List<String>.Iterator it) {
        if (!it.hasNext())
            return false; // End of tokens

        // expr -> - expr
        if (it.next().equals("-") && isOperator(it.next())) {
            if (it.next().equals("-"))
                return false;
            return expr(tokens, it);
        }

        // expr -> ( expr )
        if (it.next().equals("(")) {
            if (!expr(tokens, it))
                return false;
            if (!it.hasNext() || !it.next().equals(")"))
                return false; // Missing )
            if (it.hasNext() && isOperator(it.next())) {
                // expr -> expr op expr
                it.next(); // Consume the operator
                return expr(tokens, it);
            }
            return true;
        }

        // expr -> id
        if (isNumber(it.next())) {
            if (it.next().equals("-") && isOperator(it.next())) {
                if (it.next().equals("-"))
                    return false;
                return expr(tokens, it);
            }

            if (it.hasNext() && isOperator(it.next())) {
                // expr -> expr op expr
                it.next(); // Consume the operator
                return expr(tokens, it);
            } else {
                return true; // No more tokens or next token is not an operator
            }
        }

        return false; // Token is not an identifier or a valid expression
    }

    // Function to initiate the syntax analyzer
    static boolean SyntaxAnalyzer(List<String> tokens) {
        List<String>.Iterator it = tokens.iterator();
        return expr(tokens, it) && !it.hasNext();
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        clearScreen();

        // declaring a demo array
        String value;
        List<String> tokens = new ArrayList<>();

        while (true) {
            System.out.println("*\"E\" to terminate the program*");
            System.out.print("Enter a String: ");
            value = scanner.nextLine();

            if (value.equals("E")) {
                break;
            }

            tokens.clear();

            Tokenization(value, tokens);
            for (String token : tokens) {
                System.out.print(token);

                if (isNumber(token))
                    System.out.println(" Is a Value");
                else if (isOperator(token))
                    System.out.println(" Is an Operator");
                else if (isParenthesis(token))
                    System.out.println(" Is a Parenthesis");
                else
                    System.out.println(" Not accepted");
            }
            System.out.println("Press Enter to continue...");
            scanner.nextLine();
            clearScreen();

            // System.out.println(SyntaxAnalyzer(tokens) ? "String Accepted" : "String
            // Rejected");
            // Prompt the user to press enter to continue

        }
        return;
    }

    private static void clearScreen() {
        System.out.print("\033[H\033[2J"); // ANSI escape sequence to clear the screen
        System.out.flush();
    }

}
