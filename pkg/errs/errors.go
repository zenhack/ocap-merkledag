package errs

import (
	"fmt"
)

type ErrUnknownVariant struct {
	Type    string
	Variant uint16
}

func (e ErrUnknownVariant) Error() string {
	return fmt.Sprintf("Unknown variant %v for type %v.", e.Variant, e.Type)
}

func UnknownVariant(typ string, variant uint16) error {
	return ErrUnknownVariant{
		Type:    typ,
		Variant: variant,
	}
}
