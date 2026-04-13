#!/bin/sh
input=$(cat)

# Debug: dump raw input to inspect available fields (check from IDE to find file/selection fields)
echo "$input" | jq '.' > /tmp/claude-statusline-debug.json

model=$(echo "$input" | jq -r '.model.display_name // empty')
remaining=$(echo "$input" | jq -r '.context_window.remaining_percentage // empty')
five_hr_used=$(echo "$input" | jq -r '.rate_limits.five_hour.used_percentage // empty')
five_hr_resets=$(echo "$input" | jq -r '.rate_limits.five_hour.resets_at // empty')

# Helper: format seconds into a human-readable time remaining string
fmt_remaining() {
  secs=$1
  if [ "$secs" -le 0 ] 2>/dev/null; then
    echo "now"
    return
  fi
  days=$((secs / 86400))
  hrs=$(( (secs % 86400) / 3600 ))
  mins=$(( (secs % 3600) / 60 ))
  if [ "$days" -gt 0 ]; then
    echo "${days}d ${hrs}h"
  elif [ "$hrs" -gt 0 ]; then
    echo "${hrs}h ${mins}m"
  else
    echo "${mins}m"
  fi
}

now=$(date +%s)

parts=""

if [ -n "$model" ]; then
  parts="$model"
fi

if [ -n "$remaining" ]; then
  remaining_int=$(printf "%.0f" "$remaining")
  ctx="ctx: ${remaining_int}% left"
  if [ -n "$parts" ]; then
    parts="$parts | $ctx"
  else
    parts="$ctx"
  fi
fi

rate_parts=""
if [ -n "$five_hr_used" ]; then
  used_int=$(printf "%.0f" "$five_hr_used")
  if [ -n "$five_hr_resets" ]; then
    secs_left=$((five_hr_resets - now))
    label=$(fmt_remaining "$secs_left")
    rate_parts="5h: ${used_int}% (⏳ $label)"
  else
    rate_parts="5h: ${used_int}%"
  fi
fi

if [ -n "$rate_parts" ]; then
  if [ -n "$parts" ]; then
    parts="$parts | $rate_parts"
  else
    parts="$rate_parts"
  fi
fi

printf "%s" "$parts"
