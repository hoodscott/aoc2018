
# given list of strings, find the two strings that are most similar
# (should have a character different at one position) and return the
# common characters

proc find_close_match (f_name: string): string =
  var
    box_ids: seq[string]

  for line in lines f_name:
    box_ids.add(line)

  for checked_index, box in box_ids[0 .. box_ids.len - 2]:
    # boxes at start of array have already been checked
    # we don't need to check boxes twice
    for check_box in box_ids[checked_index + 1 .. box_ids.len - 1]:
      block checked_box:
        var
          matching_char_at = -1
        for char_index, character in box:
          if character != check_box[char_index]:
            # first time a mismatch is found, remember the position
            if matching_char_at == -1:
              matching_char_at = char_index
            # next time a mismatch is found, we can skip to the next box
            else:
              break checked_box
        # return all matching letters
        return box.substr(0, matching_char_at) & box.substr(matching_char_at + 2)

echo find_close_match("input.txt")
