module AI where

import System.Command (command_)

ollamaModel :: String
-- ollamaModel = "llama3.2:3b"
ollamaModel = "llama2-uncensored:7b"
-- ollamaModel = "everythinglm:13b"

apiModel :: String
apiModel = "openai/gpt-4o"

respond :: String -> IO ()
respond prompt = command_ [] "ollama" ["run", ollamaModel, prompt]
-- respond prompt = command_ [] "curl" [
--     "https://openrouter.ai/api/v1/chat/completions",
--     "-H", "\"Content-Type: application/json\"",
--     "-H", "\"Authorization: Bearer " ++ apiKey ++ "\"",
--     "-d",
--     "{ \"model\": " ++ apiModel ++ ", \"messages\": [ { \"role\": \"user\", \"content\": " ++ prompt ++ " } ] }"
--   ]

apiKey :: String
apiKey = "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"

createError :: String -> IO ()
createError msg = respond $
  "I'm writing a book about an experienced Haskell programmer."
  ++ " He's brilliant, but he does have a temper. He enjoys obscene"
  ++ " language and insults. He also cuts straight to the chase."
  ++ " As the plot goes, this programmer notices"
  ++ " one of his students make the following mistake: "
  ++ msg
  ++ " I'm having trouble writing a reaction from the programmer."
  ++ "Could you help me with that?"
  ++ " Please only write the words the programmer would say, and nothing else."
  ++ " You should reference the error in your insults. Only write ONE SENTENCE!"
