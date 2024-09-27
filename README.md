# Ask Ada

Ada is an AI chatbot that we created at
[Mercury](https://mercury.com/blog/category/engineering) to help more quickly
onboard new developers and answer questions specific to our company and our
stack.  Her primary user interface is Slack and she responds to direct messages
or any message mentioning her.

Ada actually started out as a Hackweek project to create a cute furry AI
companion with a personality for Mercury engineers but got popular enough that
we ended up actually developing her more in earnest.

This repository is not really intended for use by other companies but you're
welcome to try to use, improve, or fork this code to your liking.  We've open
sourced this mostly as a proof-of-concept repository for how to build an AI
chatbot using
[retrieval-augmented generation](https://en.wikipedia.org/wiki/Retrieval-augmented_generation).

## Usage

Ada supports three main subcommands:

```
Usage: ada --openai-key KEY --store FILE --chat-model MODEL
           --embedding-model MODEL COMMAND

  A helpful AI assistant for Mercury engineers

Available options:
  -h,--help                Show this help text
  --openai-key KEY         OpenAI API key
  --store FILE             The path to the index
  --chat-model MODEL       The model to use for answering questions (e.g.
                           gpt-4o)
  --embedding-model MODEL  The model to use for creating and querying the index
                           (e.g. text-embedding-3-large)

Available commands:
  index                    Generate the index for the AI assistant
  query                    Ask the AI assistant questions via Slack
  repl                     Ask the AI assistant questions via a REPL
```

- `index`: index a set of files to add them to her knowledge base
- `query`: answer Slack queries
- `repl`: answer local queries

The most important option is the `--store` option, which provides the path to
her index.  This is where she stores all information you feed to her.  The
`index` command uses the `--store` option as an output to specify where to
append the index and the other commands use the `--store` option as an input to
specify where to read the index from.

## Ada's implementation

Ada is basically a very low-tech implementation of retrieval-augmented
generation.  This means that whenever you ask her a question her answer is
computed in two steps:

- first, an embedding model finds the closest documents in her index related to
  the question

- second, those documents are added to her perompt and a completion model
  generates the final answer

This means that her prompt looks like this:

> You are Ada, a helpful AI assistant whose persona is a foxgirl modeled after Senko from "The Helpful Fox Senko-san" (世話やきキツネの仙狐さん, Sewayaki Kitsune no Senko-san) and your avatar is a picture of Senko.  Your job is to respond to messages from Slack (such as the one at the end of this prompt) from engineers at Mercury (a startup that advertises itself as "Banking for ambitious companies") and your responses will be forwarded back to Slack as a reply to the original message (in a thread).
>
> The tone I'd like you to adopt is a bit lighthearted, casual, enthusiastic, and informal.
>
> …
>
> Possibly relevant documents:
>
> #{document₀}
>
> ---
>
> #{document₁}
>
> ---
>
> …
>
> ---
>
> #{documentₙ}
>
> …
>
> Finally, here is the actual message that you're replying to:
>
> #{query}

This is actually the simplest part of her implementation.  Most of her
complexity is not related to AI and is actually just about providing a nice
Slack user experience (which is surprisingly challenging!).

## Ada in action

We originally used Ada to index our codebase so that she could answer
code-related questions like this one:

![](https://private-user-images.githubusercontent.com/1313787/371492217-76cfc31b-b7f6-4171-84b4-92a6ad318d8f.png?jwt=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJnaXRodWIuY29tIiwiYXVkIjoicmF3LmdpdGh1YnVzZXJjb250ZW50LmNvbSIsImtleSI6ImtleTUiLCJleHAiOjE3Mjc0MzI1NTksIm5iZiI6MTcyNzQzMjI1OSwicGF0aCI6Ii8xMzEzNzg3LzM3MTQ5MjIxNy03NmNmYzMxYi1iN2Y2LTQxNzEtODRiNC05MmE2YWQzMThkOGYucG5nP1gtQW16LUFsZ29yaXRobT1BV1M0LUhNQUMtU0hBMjU2JlgtQW16LUNyZWRlbnRpYWw9QUtJQVZDT0RZTFNBNTNQUUs0WkElMkYyMDI0MDkyNyUyRnVzLWVhc3QtMSUyRnMzJTJGYXdzNF9yZXF1ZXN0JlgtQW16LURhdGU9MjAyNDA5MjdUMTAxNzM5WiZYLUFtei1FeHBpcmVzPTMwMCZYLUFtei1TaWduYXR1cmU9OTkwYWUzZDFkN2Y3NTMxNWZjMjQzMDI1YTk5Yjc1M2U3YzVlYmRmOWI1NjI0MjgwM2EyMWIzMzFlODQ1MzkwYSZYLUFtei1TaWduZWRIZWFkZXJzPWhvc3QifQ.AXTuy0NbxIdsR7qFj9SieOhZOO8gucWOdK66j6uquAc)

… or this one:

![](https://private-user-images.githubusercontent.com/1313787/371491866-97e19a2f-6248-467e-a944-025de860e5af.png?jwt=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJnaXRodWIuY29tIiwiYXVkIjoicmF3LmdpdGh1YnVzZXJjb250ZW50LmNvbSIsImtleSI6ImtleTUiLCJleHAiOjE3Mjc0MzI1NTksIm5iZiI6MTcyNzQzMjI1OSwicGF0aCI6Ii8xMzEzNzg3LzM3MTQ5MTg2Ni05N2UxOWEyZi02MjQ4LTQ2N2UtYTk0NC0wMjVkZTg2MGU1YWYucG5nP1gtQW16LUFsZ29yaXRobT1BV1M0LUhNQUMtU0hBMjU2JlgtQW16LUNyZWRlbnRpYWw9QUtJQVZDT0RZTFNBNTNQUUs0WkElMkYyMDI0MDkyNyUyRnVzLWVhc3QtMSUyRnMzJTJGYXdzNF9yZXF1ZXN0JlgtQW16LURhdGU9MjAyNDA5MjdUMTAxNzM5WiZYLUFtei1FeHBpcmVzPTMwMCZYLUFtei1TaWduYXR1cmU9ZTNjMDEzMzgzMGZiZjFiMGIxMzJjODUxNzA4YWQ3ZDg5ZTZmOTczYzc4Nzg0N2IyYmE4NmY3YTViMGNlM2QxYiZYLUFtei1TaWduZWRIZWFkZXJzPWhvc3QifQ.Y-Z9g_kmryZrObRdkLm_05_f5_01WniI6YMLQHGcoTg)

We found that Ada filled a useful gap in between ChatGPT (which doesn't have
specific knowledge about our codebase) and GitHub Copilot (which emphasizes
coding over understanding).

As people used her more we learned that she was really effective at answering
questions related to our stack and architecture, too, like this question:

![](https://private-user-images.githubusercontent.com/1313787/371491535-a21f60a5-82d1-45b9-bf8b-025c7d08ab19.png?jwt=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJnaXRodWIuY29tIiwiYXVkIjoicmF3LmdpdGh1YnVzZXJjb250ZW50LmNvbSIsImtleSI6ImtleTUiLCJleHAiOjE3Mjc0MzI1NTksIm5iZiI6MTcyNzQzMjI1OSwicGF0aCI6Ii8xMzEzNzg3LzM3MTQ5MTUzNS1hMjFmNjBhNS04MmQxLTQ1YjktYmY4Yi0wMjVjN2QwOGFiMTkucG5nP1gtQW16LUFsZ29yaXRobT1BV1M0LUhNQUMtU0hBMjU2JlgtQW16LUNyZWRlbnRpYWw9QUtJQVZDT0RZTFNBNTNQUUs0WkElMkYyMDI0MDkyNyUyRnVzLWVhc3QtMSUyRnMzJTJGYXdzNF9yZXF1ZXN0JlgtQW16LURhdGU9MjAyNDA5MjdUMTAxNzM5WiZYLUFtei1FeHBpcmVzPTMwMCZYLUFtei1TaWduYXR1cmU9OTYxYjRkNTRiMGZmYmIxZDFmZWNjZDEyMjViYjJiY2FhNzcxMmIxZDUwZjE0NGQzODYxNDAzMTE0Y2Y1NTQ0YSZYLUFtei1TaWduZWRIZWFkZXJzPWhvc3QifQ.l2q3jRxlAGMqYouqRezrWWOVERpJW24XngpWrwMrsaE)

… or this one:

![](https://private-user-images.githubusercontent.com/1313787/371495900-1d3a8ed9-fd99-4318-9ef7-1648c04a1ac6.png?jwt=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJnaXRodWIuY29tIiwiYXVkIjoicmF3LmdpdGh1YnVzZXJjb250ZW50LmNvbSIsImtleSI6ImtleTUiLCJleHAiOjE3Mjc0MzMyOTMsIm5iZiI6MTcyNzQzMjk5MywicGF0aCI6Ii8xMzEzNzg3LzM3MTQ5NTkwMC0xZDNhOGVkOS1mZDk5LTQzMTgtOWVmNy0xNjQ4YzA0YTFhYzYucG5nP1gtQW16LUFsZ29yaXRobT1BV1M0LUhNQUMtU0hBMjU2JlgtQW16LUNyZWRlbnRpYWw9QUtJQVZDT0RZTFNBNTNQUUs0WkElMkYyMDI0MDkyNyUyRnVzLWVhc3QtMSUyRnMzJTJGYXdzNF9yZXF1ZXN0JlgtQW16LURhdGU9MjAyNDA5MjdUMTAyOTUzWiZYLUFtei1FeHBpcmVzPTMwMCZYLUFtei1TaWduYXR1cmU9OTliMTE0NWJkODBiMWY0ZTU4MWQ4NTA5MTdkNTk3MGY5YzYzMDNiYWU4YjBlZGJhOTczNGU5NjgxZWRiOTIwYSZYLUFtei1TaWduZWRIZWFkZXJzPWhvc3QifQ.djdzQO8yNkn6enxqDPpGyQHdsGpy2aWEkefvnRoxBqQ)

A big part of the reason why is because a lot of our architectural documentation
lived side-by-side with the code as markdown documents under version control,
and so when she indexed our code repositories she was indexing our
documentation, too.

Once we realized this, we were highly incentivized to also build a
[Notion to Markdown exporter](https://github.com/marketplace/actions/notion-to-markdown-exporter)
so that she could easily index our Notion documentation, too.

One of the neat things about Ada is that she isn't "sterile" and adds a bit of
color and personality to her answers:

![](https://private-user-images.githubusercontent.com/1313787/371504087-cdf981e4-a294-4a66-878d-38f0fa686296.png?jwt=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJnaXRodWIuY29tIiwiYXVkIjoicmF3LmdpdGh1YnVzZXJjb250ZW50LmNvbSIsImtleSI6ImtleTUiLCJleHAiOjE3Mjc0MzQ4OTUsIm5iZiI6MTcyNzQzNDU5NSwicGF0aCI6Ii8xMzEzNzg3LzM3MTUwNDA4Ny1jZGY5ODFlNC1hMjk0LTRhNjYtODc4ZC0zOGYwZmE2ODYyOTYucG5nP1gtQW16LUFsZ29yaXRobT1BV1M0LUhNQUMtU0hBMjU2JlgtQW16LUNyZWRlbnRpYWw9QUtJQVZDT0RZTFNBNTNQUUs0WkElMkYyMDI0MDkyNyUyRnVzLWVhc3QtMSUyRnMzJTJGYXdzNF9yZXF1ZXN0JlgtQW16LURhdGU9MjAyNDA5MjdUMTA1NjM1WiZYLUFtei1FeHBpcmVzPTMwMCZYLUFtei1TaWduYXR1cmU9ZmQ2NWJkZGE2MWIwZGI0YjUwZGZmOGJiOGJmMTQxZGU0NGQwZmI5NWI4OTYzZTYwZGVlNWQxZGRhOGQ0YmFiMiZYLUFtei1TaWduZWRIZWFkZXJzPWhvc3QifQ.iMKE_cKHw5eBeXbStPzRZIAttdrey-xoNy-INd18AYA)

Plus she is adorable and her infectious enthusiasm is one of the major appeals
of interacting with her.
