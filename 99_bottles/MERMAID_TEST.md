# Mermaid Test

This is a simple test to verify Mermaid rendering works on GitHub.

## Simple Sequence Diagram

```mermaid
sequenceDiagram
    participant A as Alice
    participant B as Bob
    A->>B: Hello Bob!
    B-->>A: Hi Alice!
```

## More Complex Diagram

```mermaid
sequenceDiagram
    participant User
    participant System
    User->>System: request()
    System-->>User: response()
```

**Note**: These diagrams will render automatically on GitHub. If viewing locally, you may need a Mermaid preview extension.
