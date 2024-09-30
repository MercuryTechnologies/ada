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

- second, those documents are added to her prompt and a completion model
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

![](./images/question0.png)

… or this one:

![](./images/question1.png)

We found that Ada filled a useful gap in between ChatGPT (which doesn't have
specific knowledge about our codebase) and GitHub Copilot (which emphasizes
coding over understanding).

As people used her more we learned that she was really effective at answering
questions related to our stack and architecture, too, like this question:

![](./images/question2.png)

… or this one:

![](./images/question3.png)

A big part of the reason why is because a lot of our architectural documentation
lived side-by-side with the code as markdown documents under version control,
and so when she indexed our code repositories she was indexing our
documentation, too.

Once we realized this, we were highly incentivized to also build a
[Notion to Markdown exporter](https://github.com/marketplace/actions/notion-to-markdown-exporter)
so that she could easily index our Notion documentation, too.

One of the neat things about Ada is that she isn't "sterile" and adds a bit of
color and personality to her answers:

![](./images/question4.png)

Plus she is adorable and her infectious enthusiasm is one of the major appeals
of interacting with her.
