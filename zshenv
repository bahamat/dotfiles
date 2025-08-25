#   Copyright 2025 Brianna Bennett
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

if [[ $OSTYPE =~ darwin ]]; then
    ssh-add -l >/dev/null || ssh-add --apple-load-keychain 2>/dev/null
fi
export GPG_TTY=$(tty)
export GPG_AGENT_INFO=$HOME/.gnupg/S.gpg-agent

[[ -f ~/.cargo/env ]] && source ~/.cargo/env

export RUST_LOG_PRETTY="true"
