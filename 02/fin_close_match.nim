import strutils

var
  box_ids: seq[string]
  matching_char_at: int

for line in lines "input.txt":
  box_ids.add(line)

block compare_boxes:
  for checked_pointer, box in box_ids[0 .. box_ids.len - 2]:
    for check_box in box_ids[checked_pointer + 1 .. box_ids.len - 1]:
      block checked_box:
        # boxes at start of array have already been checked
        # we don't need to check boxes twice
        var matching_char_at = -1
        for index, character in box:
          if character != check_box[index]:
            # first time a mismatch is found, remember the position
            if matching_char_at == -1:
              matching_char_at = index
            # next time a mismatch is found, we can skip to the next box
            else:
              break checked_box
        # return all matching letters
        echo box.substr(0,matching_char_at) & box.substr(matching_char_at + 2)
        break compare_boxes
