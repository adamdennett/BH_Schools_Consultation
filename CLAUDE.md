# BH_Schools_Consultation

**Outstanding:** read `MACHINE_SYNC_TODO.md` first. There are setup fixes to
make on the main machine (where the git-ignored data lives) so the repo runs
from a fresh clone. Mention it to the user at the start of a session until
it's done.

- Never use absolute drive paths like `E:/...`. Use `here::here()` for this
  repo and `dirname(here::here())` for sibling repos.
